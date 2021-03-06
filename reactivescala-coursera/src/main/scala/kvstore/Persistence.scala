package kvstore

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{OneForOneStrategy, Actor, Props}

import scala.util.Random

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException extends Exception("Persistence failure")

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}

class Persistence(flaky: Boolean) extends Actor {
  import kvstore.Persistence._

  override val supervisorStrategy = OneForOneStrategy() {
    case ex: Exception => println(ex.getMessage)
      Restart
  }

  def receive = {
    case Persist(key, _, id) =>
      if (!flaky || Random.nextBoolean()) sender ! Persisted(key, id)
      else throw new PersistenceException
  }

}
