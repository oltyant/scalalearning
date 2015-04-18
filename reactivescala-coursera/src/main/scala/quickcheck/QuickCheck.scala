package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Gen._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(elem, h)

  lazy val genHeap2: Gen[H] = arbitrary[A].flatMap(elem => oneOf(const(empty), genHeap).map( h => insert(elem, h)))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap2)

  property("findMin on a heap that contain one int should give back that int") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("1. If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(h2) == math.min(a, b)
  }

  property("2. If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty") = forAll {
    a: Int =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }

  property("3. Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimal") = forAll {
    h: H =>
      @annotation.tailrec
      def loop(h: H, lastResult: Int): Boolean = h match {
        case List() => true
        case _ => {
          val min = findMin(h)
          min >= lastResult && (loop(deleteMin(h), findMin(h)))
        }
      }
      if (isEmpty(h)) true
      else loop(deleteMin(h), findMin(h))
  }

  property("4. Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll(genHeap, genHeap2) {
    (h1: H, h2: H) =>
      val melded = meld(h1, h2)
      if (isEmpty(melded)) true
      else {
        //then one of the input heaps must be not empty
        val h1min = if (isEmpty(h1)) findMin(h2) else findMin(h1)
        val h2min = if (isEmpty(h2)) findMin(h1) else findMin(h2)
        findMin(melded) == math.min(h1min, h2min)
      }
  }

  property("a melded heap must contain the two input heaps elements in order") = forAll(genHeap, genHeap2) {
    (heap: H, heap2: H) =>
      @annotation.tailrec
      def loop(melded: H, h1: H, h2: H): Boolean = (melded, h1, h2) match {
        case (List(), _, _) => true
        case (_, _, List()) => findMin(h1) == findMin(melded) && loop(deleteMin(melded), deleteMin(h1), h2)
        case (_, List(), _) => findMin(h2) == findMin(melded) && loop(deleteMin(melded), h1, deleteMin(h2))
        case _ => {
          val min = math.min(findMin(h1), findMin(h2))
          min == findMin(melded) && (if (min == findMin(h1)) loop(deleteMin(melded), deleteMin(h1), h2) else loop(deleteMin(melded), h1, deleteMin(h2)))
        }
      }
      loop(meld(heap, heap2), heap, heap2)
  }

  property("reinsertion of the minimum element won't affect the findMin result") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin from a two length heap") = forAll {
    (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      deleteMin(h) == insert(math.max(a, b), empty)
      deleteMin(deleteMin(h)) == empty
  }

  property("meld should give back the input heap if the other heap is empty") = forAll {
    (h: H) =>
      meld(empty, h) == h && meld(h, empty) == h
  }

}
