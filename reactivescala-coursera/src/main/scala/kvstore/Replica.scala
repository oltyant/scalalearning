package kvstore

import akka.actor.SupervisorStrategy.Restart
import akka.actor._
import akka.event.LoggingReceive
import kvstore.Arbiter._
import kvstore.Persistence.{Persist, Persisted}
import kvstore.Replicator.{Replicate, Replicated, Snapshot, SnapshotAck}

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import akka.pattern.ask
  import context.dispatcher
  import kvstore.Replica._

import scala.concurrent.Future

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  var cancellables = Map.empty[Long, (ActorRef, Cancellable)]
//  var globalAcks = Map.empty[ActorRef, Long]

  var expectedSeq = 0L
  var count = 0L

  override val supervisorStrategy = OneForOneStrategy() {
    case ex: Exception => println(ex.getMessage)
      Restart
  }

  val persister = context.system.actorOf(persistenceProps)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = LoggingReceive {
    case Insert(key, value, id) =>
      kv += key -> value
      updateReplicators(None, Some(key -> value), id)
      askPersist(key, Some(value), id)
    case Remove(key, id) => kv -= key
      updateReplicators(remove = Some(key), id = id)
      askPersist(key, None, id)
    case get @ Get(key, id) => getter(get)
    case r @ Replicas(replicas) => updateReplicas(replicas)
    case r @ Replicated(key, id) =>
    case Terminated(ref) => terminate(ref)
  }

  val replica: Receive = LoggingReceive {
    case get @ Get(key, id) => getter(get)
    case Snapshot(k, v, seq) =>
      if (seq < expectedSeq) {
        sender ! SnapshotAck(k, seq)
//        globalAcks -= sender
        expectedSeq = math.max(expectedSeq, seq + 1)
      } else if (seq == expectedSeq) {
        v match {
          case None => kv -= k
            val c = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persister, Persist(k, None, seq))
            cancellables += seq -> (sender, c)
          case Some(value) => kv += k -> value
            val c = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persister, Persist(k, Some(value), seq))
            cancellables += seq -> (sender, c)
        }
        expectedSeq = math.max(expectedSeq, seq + 1)
      }
    case Persisted(key, seq) => cancellables get seq match {
      case Some((s: ActorRef, c: Cancellable)) => c.cancel()
        s ! SnapshotAck(key, seq)
//        globalAcks -= s
      case _ =>
    }
    case Terminated(ref: ActorRef) => terminate(ref)
  }

  def askPersist(key: String, value: Option[String] = None, id: Long) {
    def loop: Try[Persisted] = {
      val f: Future[Persisted] = ask(persister, Persist(key, value, id))(100 milliseconds).mapTo[Persisted]
      Try(Await.result(f, Duration.Inf))
    }
    val persisted = Promise[Persisted]
    val cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds){
      if (! persisted.isCompleted) loop match {
        case s @ Success(Persisted(key, id)) => persisted.success(Persisted(key, id))
        case Failure(ex) =>
      }}
    Await.result(akka.pattern.after(1 seconds, context.system.scheduler)(Future {
      cancellable.cancel()
      if (persisted.isCompleted
//        && globalAcks.filter{case (rep: ActorRef, i: Long) => i == id}.isEmpty
          ) sender ! OperationAck(id) else sender ! OperationFailed(id)
    }), Duration.Inf)
  }

  def getter(g: Get) = {
    val res: Option[String] = kv get g.key
    sender ! GetResult(g.key, res, g.id)
  }

  private def updateReplicas(replicas: Set[ActorRef]) {
    val all = replicas - self
    val newOnes = all -- secondaries.keySet
    val diers = secondaries.keySet -- all

    all.foreach(context.watch)
    newOnes.foreach { replica =>
      val replicator = context.actorOf(Replicator.props(replica))
      replicators += replicator
      kv.foreach {
        case (k, v) => {
          replicator ! Replicate(k, Some(v), count)
        }
      }
      secondaries += replica -> replicator
    }
    count += 1

    diers.foreach { replica => {
        secondaries get replica match {
          case Some(r) =>
            replicators -= r
            r ! PoisonPill
          case None =>
        }
        replica ! PoisonPill
        secondaries -= replica
      }
    }
  }

  private def updateReplicators(remove: Option[String] = None, insert: Option[(String, String)] = None, id: Long): Unit = {
    secondaries.foreach { case (secondary, replicator) =>
      if (insert.nonEmpty) {
//        globalAcks += replicator -> id
        replicator ! Replicate(insert.get._1, Some(insert.get._2), id)
      } else if (remove.nonEmpty) {
        //globalAcks += replicator -> id
        replicator ! Replicate(remove.get, None, id)
      } else {
        println("invalid update Replicator operation")
        throw new Exception("invalid update Replicator operation")
      }
    }
  }

  private def terminate(ref: ActorRef) {
    context.unwatch(ref)
    context.stop(ref)
    if (secondaries get ref nonEmpty) {
      context.stop(secondaries(ref))
      replicators -= (secondaries(ref))
      secondaries -= ref
    }
  }

}

