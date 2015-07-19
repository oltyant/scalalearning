/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import actorbintree.BinaryTreeSet._
import akka.actor._
import akka.event.LoggingReceive

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import actorbintree.BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = LoggingReceive {
    case i @ Insert(req, id, e) => root ! i
    case c @ Contains(req, id, e) => root ! c
    case r @ Remove(req, id, e) => root ! r
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = LoggingReceive {
    case GC =>
    case op: Operation => pendingQueue = pendingQueue.enqueue(op)
    case CopyFinished =>
      root ! PoisonPill
      root = newRoot
      context become normal
      pendingQueue = pendingQueue.flatMap(op => { root ! op; Queue.empty[Operation]})
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import actorbintree.BinaryTreeNode._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = LoggingReceive {
    case op @ Insert(req, id, e) => {
      if (e == elem) {
        removed = false; req ! OperationFinished(id)
      } else if (e < elem && subtrees.get(Left) != None) subtrees(Left) ! op
      else if (e < elem ) {
        subtrees += (Left -> context.actorOf(BinaryTreeNode.props(e, false)))
        req ! OperationFinished(id)
      }
      else if (e > elem && subtrees.get(Right) != None) subtrees(Right) ! op
      else {
        subtrees += (Right -> context.actorOf(BinaryTreeNode.props(e, false)))
        req ! OperationFinished(id)
      }
    }
    case op @ Contains(req, id, e) => {
      if (e == elem) {
        req ! ContainsResult(id, !removed)
      } else if (e < elem && subtrees.get(Left) != None) {
        subtrees(Left) ! op
      } else if (e < elem) {
        req ! ContainsResult(id, false)
      } else if (e > elem && subtrees.get(Right) != None) {
        subtrees(Right) ! op
      } else {
        req ! ContainsResult(id, false)
      }
    }
    case op @ Remove(req, id, e) => {
      if (e == elem) {
        removed = true
        req ! OperationFinished(id)
      } else if (e < elem && subtrees.get(Left) != None) {
        subtrees(Left) ! op
      } else if (e < elem) {
        req ! OperationFinished(id)
      } else if (e > elem && subtrees.get(Right) != None) {
        subtrees(Right) ! op
      } else {
        req ! OperationFinished(id)
      }
    }
    case op @ ContainsResult(id, contains) => context.parent ! op
    case op @ OperationFinished(id) => context.parent ! op
    case op @ CopyTo(node) => {
      if (!removed) node ! Insert(self, elem, elem)
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
        context become normal
      } else {
        context.become(copying(subtrees.values.map {
          ref => {
            ref ! op
            ref
          }
        }.toSet, removed))
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = LoggingReceive {
    case OperationFinished(id) => {
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
      }
      else context become copying(expected, true)
    }
    case CopyFinished => {
      val rest = expected - sender
      if (rest.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        self ! PoisonPill
      }
      else context become copying(rest, insertConfirmed)
    }
  }


}
