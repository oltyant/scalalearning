package practice.datastructures

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

import scala.{List, Nil => _}
import scala.collection.immutable.{List, Nil => _}

import practice.datastructures._
import org.specs2.mutable.Specification

/**
 * Created by oltyant on 3/15/15.
 */
object TestLists extends Specification {
  "The 'sum' method" should {
    "give back zero when the list is empty" in {
      val l = List()
      val result = List.sum(l)
      result mustEqual 0
    }
    "give back 10 when the list contain -20, 10, 30" in {
      val l = List(-20, 10, 20)
      val result = List.sum(l)
      result mustEqual 10
    }
  }
  "The 'product' method" should {
    "give back one when the list is empty" in {
      val l = List()
      val result = List.product(l)
      result mustEqual 1.0
    }
    "give back 10 when the list contain -2.0, 5.0, -1.0" in {
      val l = List(-2.0, 5.0, -1.0)
      val result = List.product(l)
      result mustEqual 10.0
    }
  }
  "The 'tail' method" should {
    "give back the empty list when the given list is empty" in {
      val l = List()
      val result = List.tail(l)
      result mustEqual List()
    }
    "give back Nil when the given list is Nil" in {
      val l = Nil
      val result = List.tail(l)
      result mustEqual Nil
    }
    "give back the given list without the head otherwise" in {
      val l = List(-1,2,56,1231,2,22,1,-1)
      val result = List.tail(l)
      result mustEqual List(2,56,1231,2,22,1,-1)
    }
  }
  "The 'setHead' method" should {
    "raise NoSuchElementException when the given list is empty or Nil" in {
      val l1 = List()
      val e = 1
      List.setHead(l1, e) must throwA[NoSuchElementException]
      val l2 = Nil
      List.setHead(l2, e) must throwA[NoSuchElementException]
    }
    "give back the input list but with the given head value otherwise" in {
      val l = List(-1,2,56,1231,2,22,1,-1)
      val e = 1
      val result = List.setHead(l, e)
      result mustEqual List(1, 2,56,1231,2,22,1,-1)
    }
  }
  "The 'drop' method" should {
    "give back the original list when it is empty or Nil" in {
      val l1 = List()
      val e = 10
      List.drop(l1, e) mustEqual l1
      val l2 = Nil
      List.drop(l2, e) mustEqual l2
    }
    "give back the input list but without the first n element" in {
      val l = List(-1,2,56,1231,2,22,1,-1)
      val e = 3
      val result = List.drop(l, e)
      result mustEqual List(1231,2,22,1,-1)
    }
  }
}

object CheckLists extends Properties("List") {
  //We need to filter out big and small doubles as the product result seems too variadic for the list of really big or really small numbers
  //that is because of the double's byte representation
  def filterDoubleSeq(base: Gen[Seq[Double]]) = base.map(_.filter(x => (math.abs(x) > "1.0E-5".toDouble) && (math.abs(x) < "1.0E5".toDouble))).filter(_.size < 5)
  import Arbitrary.arbitrary
  //we need to generate seq as the scala's list is hidden
  //the arbirtray's and Gen's builders are implicit's
  //and the implementation of a reasonable List's implicit is too complicated for testing reason
  val genIntSeqs: Gen[Seq[Int]] = arbitrary[Seq[Int]]
  val genDoubleSeq: Gen[Seq[Double]] = filterDoubleSeq(arbitrary[Seq[Double]])

  property("sum of an int list") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val list: List[Int] = List(seq: _*)
      List.sum(list) == List.foldLeft(list, 0)(_ + _)
    }
  }

  property("product of a double list") = forAll(genDoubleSeq) {
    (seq: Seq[Double]) => {
      val list: List[Double] = List(seq: _*)
      val prod = List.product(list)
      val fold = List.foldLeft(list, 1.0)(_ * _)
      val diff = math.abs(List.product(list) - List.foldLeft(list, 1.0)(_ * _))
      if (prod.equals(Double.PositiveInfinity) || prod.equals(Double.NegativeInfinity))
        (fold equals Double.PositiveInfinity) || (fold equals Double.NegativeInfinity)
      else diff <= "1.0E-5".toDouble || diff > "1.0E10".toDouble
    }
  }

  property("tail of a list") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val list: List[Int] = List(seq: _*)
      val expected = List.drop(list, 1)
      expected == List.tail(list)
    }
  }

  property("list setHead") = forAll(genIntSeqs, arbitrary[Int]) {
    (seq: Seq[Int], e: Int) => {
      val list: List[Int] = List(seq: _*)
      list match {
        case Nil => true //as the call of setHead throw NoSuchElementException
        case l: Cons[Int] => List.setHead(l, e) == Cons(e, List.drop(l, 1))
      }
    }
  }

  property("drop the list's first n element") = forAll(genIntSeqs, arbitrary[Int]) {
    (seq: Seq[Int], e: Int) => {
      val list: List[Int] = List(seq: _*)
      List.drop(list, e) == List(seq.drop(e): _*)
    }
  }
}
