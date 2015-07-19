package kvstore

import akka.actor.{Actor, ActorRef, Cancellable, Props}

import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val secondary: ActorRef) extends Actor {
  import kvstore.Replicator._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Map.empty[Long, Long]

  var cancellables = Map.empty[Long, Cancellable]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  def receive: Receive = {
    case rep @ Replicate(key, valueOption, id) =>
      val snapShotMsg = Snapshot(key, valueOption, _seqCounter)
      acks += _seqCounter -> (sender, rep)
      cancellables += id -> context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, secondary, snapShotMsg)
      nextSeq
    case SnapshotAck(k, seq) => acks get seq match {
        case Some((s: ActorRef, Replicate(key, valueOption, id))) => cancellables get id match {
            case Some(c: Cancellable) => c.cancel()
            case _ =>
          }
          s ! Replicated(key, id)
        case _ =>
    }
  }

}
