package practice.datastructures

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import org.specs2.mutable.Specification

import scala.collection.immutable.{Nil => _}
import scala.{Nil => _}

/**
 * Created by oltyant on 3/15/15.
 */
object TestLists extends Specification {
  private lazy val millionLengthIntSeq = (1 to 1000000).toSeq
  private lazy val millionLengthIntList = List(millionLengthIntSeq: _*)
  private lazy val millionLengthDoubleList = List(millionLengthIntSeq.map(_.toDouble): _*)
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
  "The 'dropWhile' method" should {
    "give back the original list when it is empty or Nil" in {
      val l1 = List()
      val criterion: Int => Boolean = _ % 2 == 0
      List.dropWhile(l1, criterion) mustEqual l1
      val l2 = Nil
      List.dropWhile(l2, criterion) mustEqual l2
    }
    "give back the original list when none of the elements fullfill the criterion" in {
      val l = List(1,2,3,4,6,7,4321)
      val criterion: Int => Boolean = _ < 0
      List.dropWhile(l, criterion) mustEqual l
    }
    "give back the list that is constructed from the last 3 element that is not fulfill the criterion" in {
      val l = List(1,2,-34,0,-56)
      val criterion = (a: Int) => a > 0
      List.dropWhile(l, criterion) mustEqual List(-34,0,-56)
    }
  }
  "The 'add' method" should {
    "give back Nil when the given lists are empty or Nil" in {
      val l1 = Nil
      val l2 = List()
      List.add(l1, l2) mustEqual Nil
      List.add(l2, l1) mustEqual Nil
      List.add(l1, l1) mustEqual Nil
      List.add(l2, l2) mustEqual Nil
    }
    "give back the second list if the first list is Nil (or empty)" in {
      val l1 = Nil
      val l2 = List(1,2,3,4)
      List.add(l1, l2) mustEqual l2
    }
    "give back the first list if the second list is Nil (or empty)" in {
      val l1 = List(1,2,3,4)
      val l2 = Nil
      List.add(l1, l2) mustEqual l1
    }
    "give back the concatenation of the given lists if none of the inputs empty or Nil" in {
      val l1 = List(1,2,3)
      val l2 = List(4,5,6)
      List.add(l1, l2) mustEqual List(1,2,3,4,5,6)
    }
  }
  "The 'init' method" should {
    "give back Nil when the List is Nil" in {
      val l = Nil
      List.init(l) mustEqual Nil
    }
    "give back Nil when the input List only contains one element" in {
      val l = List(new Object())
      List.init(l) mustEqual Nil
    }
    "- aside from the last element - it gives back the original list" in {
      val l = List(0,1,2,3,4,5,6)
      List.init(l) mustEqual List(0,1,2,3,4,5)
    }
  }
  "The 'foldRight' method" should {
    "give back 0 when the zero element is 0 and the given list is empty" in {
      List.foldRight(List[Int](), 0)(_ + _) mustEqual 0
    }
    "give back 0 when the List is not empty but the zero element is 0 and the function is the product" in {
      List.foldRight(List(1,-6,7,8,11,34), 0)(_ * _) mustEqual 0
    }
    "give back 5 factorial whenthe List is a range from 1 to 5, the zero element is 1 and the function is the product" in {
      List.foldRight(List(1,2,3,4,5), 1)(_ * _) mustEqual 120
    }
    "throw a StackOverflowError when we try to make the sum of million element" in {
      List.foldRight(millionLengthIntList, 0)(_ + _) must throwA[StackOverflowError]
    }
  }
  "The 'sum2' method" should {
    "give back zero when the list is empty" in {
      val l = List()
      val result = List.sum2(l)
      result mustEqual 0
    }
    "give back 10 when the list contain -20, 10, 30" in {
      val l = List(-20, 10, 20)
      val result = List.sum2(l)
      result mustEqual 10
    }
    "throw StackOverflowError when the given list contains 1 million elements" in {
      List.sum2(millionLengthIntList) must throwA[StackOverflowError]
    }
  }
  "The 'product2' method" should {
    "give back one when the list is empty" in {
      val l = List()
      val result = List.product2(l)
      result mustEqual 1.0
    }
    "give back 10 when the list contain -2.0, 5.0, -1.0" in {
      val l = List(-2.0, 5.0, -1.0)
      val result = List.product2(l)
      result mustEqual 10.0
    }
    "throw StackOverflowError when the given list contains 1 million elements" in {
      List.product2(millionLengthDoubleList) must throwA[StackOverflowError]
    }
  }
  "The 'length' method" should {
    "give back zero when the list is empty" in {
      val l = List()
      val result = List.length(l)
      result mustEqual 0
    }
    "give back 3 when the list contain 3 elements" in {
      val l = List(-2.0, 5.0, -1.0)
      val result = List.length(l)
      result mustEqual 3
    }
    "throw StackOverflowError when the given list contains 1 million elements" in {
      List.length(millionLengthIntList) must throwA[StackOverflowError]
    }
  }
  "The 'foldLeft' method" should {
    "give back 0 when the zero element is 0 and the given list is empty" in {
      List.foldLeft(List[Int](), 0)(_ + _) mustEqual 0
    }
    "give back 0 when the List is not empty but the zero element is 0 and the function is the product" in {
      List.foldLeft(List(1,-6,7,8,11,34), 0)(_ * _) mustEqual 0
    }
    "give back 5 factorial whenthe List is a range from 1 to 5, the zero element is 1 and the function is the product" in {
      List.foldLeft(List(1,2,3,4,5), 1)(_ * _) mustEqual 120
    }
    "not throw a StackOverflowError when we try to make the sum of million element" in {
      List.foldLeft2(millionLengthIntList, 0)(_ + _) must not(throwA[StackOverflowError])
    }
  }
  "The 'sum3' method" should {
    "give back zero when the list is empty" in {
      val l = List()
      val result = List.sum3(l)
      result mustEqual 0
    }
    "give back 10 when the list contain -20, 10, 30" in {
      val l = List(-20, 10, 20)
      val result = List.sum3(l)
      result mustEqual 10
    }
    "does not throw StackOverflowError when the given list contains 1 million elements" in {
      List.sum3(millionLengthIntList) must not(throwA[StackOverflowError])
    }
  }
  "The 'product3' method" should {
    "give back one when the list is empty" in {
      val l = List()
      val result = List.product3(l)
      result mustEqual 1.0
    }
    "give back 10 when the list contain -2.0, 5.0, -1.0" in {
      val l = List(-2, 5, -1)
      val result = List.product3(l)
      result mustEqual 10.0
    }
    "not throw StackOverflowError when the given list contains 1 million elements" in {
      List.product3(millionLengthIntList) must not(throwA[StackOverflowError])
    }
  }
  "The 'reverse' method" should {
    "give back the empty list if the input list is empty" in {
      List.reverse(List()) mustEqual List()
    }
    "give back the reverse of the input list otherwise" in {
      List.reverse(List(1,2,3)) mustEqual List(3,2,1)
    }
  }
  "The 'foldRight2' method" should {
    "give back 0 when the zero element is 0 and the given list is empty" in {
      List.foldRight2(List[Int](), 0)(_ + _) mustEqual 0
    }
    "give back 0 when the List is not empty but the zero element is 0 and the function is the product" in {
      List.foldRight2(List(1,-6,7,8,11,34), 0)(_ * _) mustEqual 0
    }
    "give back 5 factorial whenthe List is a range from 1 to 5, the zero element is 1 and the function is the product" in {
      List.foldRight2(List(1,2,3,4,5), 1)(_ * _) mustEqual 120
    }
    "not throw a StackOverflowError when we try to make the sum of million element" in {
      List.foldRight2(millionLengthIntList, 0)(_ + _) must not(throwA[StackOverflowError])
    }
  }
  "The 'append' method" should {
    "give back the list only contains the appended element when the given list is empty" in {
      List.append(List[Int](), 1) mustEqual List(1)
    }
    "give back the input list with the given element appended to the end otherwise" in {
      List.append(List(1,2,3), -1) mustEqual List(1,2,3,-1)
    }
  }
  "The 'flatten' method" should {
    "give back Nil when the given list only contain empty lists" in {
      List.flatten(List(List(), Nil)) mustEqual Nil
    }
    "give back the flatten list otherwise" in {
      List.flatten(List(List(1,2), Nil, List(3,4), Nil)) mustEqual List(1,2,3,4)
    }
  }
  "The 'add1' method" should {
    "give back Nil if the input list is Nil" in {
      List.add1(Nil) == Nil
    }
    "give back a list with incremented elements compared to the input list" in {
      List.add1(List(0,1,2,3,4)) == List(1,2,3,4,5)
    }
  }
  "The 'doubleToString' method" should {
    "give back Nil when the input list is Nil" in {
      List.doubleToString(Nil) == Nil
    }
    "give back the input list but with the applied toString elements" in {
      List.doubleToString(List(1.0, 2.0, 3.4)) == List(Seq(1.0, 2.0, 3.4).map(_.toString()): _*)
    }
    "throw StackOverflowError when we have one million element in the list" in {
      List.doubleToString(millionLengthDoubleList) must throwA[StackOverflowError]
    }
  }
  "The 'doubleToStringFold' method" should {
    "give back Nil when the input list is Nil" in {
      List.doubleToStringFold(Nil) == Nil
    }
    "give back the input list but with the applied toString elements" in {
      List.doubleToStringFold(List(1.0, 2.0, 3.4)) == List(Seq(1.0, 2.0, 3.4).map(_.toString()): _*)
    }
    "not throw StackOverflowError when we have one million element in the list" in {
      List.doubleToStringFold(millionLengthDoubleList) must not(throwA[StackOverflowError])
    }
  }
  "The 'filter' method" should {
    "give back Nil when the input list is Nil" in {
      List.filter(List[Int]())(_ == 0) == Nil
    }
    "give back Nil when no one element fulfill the given criterion" in {
      List.filter(List(1,2,3,4,5))(_ < 0) == Nil
    }
    "give back the input list when all of the elements fulfill the given criterion" in {
      List.filter(List(1,2,3,4,5))(_ > 0) == List(1,2,3,4,5)
    }
    "give back the odd elements when the criterion only fulfill to the odd elements" in {
      List.filter(List(1,2,3,4,5))(_ % 2 == 1) == List(1,3,5)
    }
  }
}

object CheckLists extends Properties("List") {
  //We need to filter out big and small doubles as their product result seems too variadic for the list of really big or really small numbers
  //that is because of the double's byte representation
  def filterDoubleSeq(base: Gen[Seq[Double]]) = base.map(_.filter(x => (math.abs(x) > "1.0E-5".toDouble) && (math.abs(x) < "1.0E5".toDouble))).filter(_.size < 5)
  import org.scalacheck.Arbitrary.arbitrary
  //we need to generate seq as the scala's list is hidden
  //the arbirtray's and Gen's builders are implicit's
  //and the implementation of a reasonable List's implicit is too complicated for testing reason
  lazy val genIntSeqs: Gen[Seq[Int]] = arbitrary[Seq[Int]]
  lazy val genDoubleSeq: Gen[Seq[Double]] = filterDoubleSeq(arbitrary[Seq[Double]])
  lazy val genSeqOfIntSeqs: Gen[Seq[Seq[Int]]] = arbitrary[Seq[Seq[Int]]]

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

  property("drop the list's first n element while these fulfill the criterion but the n + 1th is not") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val list = List(seq: _*)
      val greaterThanHundred = (a: Int) => a > 100
      List.dropWhile(list, greaterThanHundred) == List(seq.dropWhile(greaterThanHundred): _*)
    }
  }

  property("concatenate two lists") = forAll(genIntSeqs, arbitrary[Seq[Int]]) {
    (seq1: Seq[Int], seq2: Seq[Int]) => {
      val list1 = List(seq1: _*)
      val list2 = List(seq2: _*)
      List.add(list1, list2) == List(seq1 ++ seq2: _*)
    }
  }

  property("remove the last element from a List") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      List.init(List(seq: _*)) == List(seq.dropRight(1): _*)
    }
  }

  property("fold the list to right and sum its values") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val adder: (Int, Int) => Int = _ + _
      List.foldRight(List(seq: _*), 0)(adder) == seq.foldRight(0)(adder)
    }
  }

  property("fold the list to left and sum its values") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val adder: (Int, Int) => Int = _ + _
      List.foldLeft(List(seq: _*), 0)(adder) == seq.foldLeft(0)(adder)
    }
  }

  property("reverse of a given list") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val l = List(seq: _*)
      List.reverse(l) == List(seq.reverse: _*)
    }
  }
  
  property("fold the list to right and sum its values (tail recursive)") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val adder: (Int, Int) => Int = _ + _
      List.foldRight2(List(seq: _*), 0)(adder) == seq.foldRight(0)(adder)
    }
  }

  property("flatten method") = forAll(genSeqOfIntSeqs) {
    (seq: Seq[Seq[Int]]) => {
      val ll: List[List[Int]] = List(seq.map((x: Seq[Int]) => List(x: _*)): _*)
      List.flatten(ll) == List(seq.flatten: _*)
    }
  }

  property("map method") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val l = List(seq: _*)
      val f: Int => String = _.toString
      List.map(l, f) == List(seq.map(f): _*)
    }
  }

  property("filter method") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val l = List(seq: _*)
      val criterion: Int => Boolean = _ % 2 == 0
      List.filter(l)(criterion) == List(seq.filter(criterion): _*)
    }
  }

  property("flatMap method") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val l = List(seq: _*)
      val f: Int => Seq[Int] = x => Seq(x, x * x)
      val g: Int => List[Int] = x => List(f(x): _*)
      List.flatMap(l)(g) == List(seq.flatMap(f): _*)
    }
  }

  property("Filter with flatMap") = forAll(genIntSeqs) {
    (seq: Seq[Int]) => {
      val l = List(seq: _*)
      val criterion = (x: Int) => x % 2 == 0
      List.filterFM(l)(criterion) == List(seq.filter(criterion): _*)
    }
  }

  property("merge two lists") = forAll(genIntSeqs, arbitrary[Seq[Int]]) {
    (seq1: Seq[Int], seq2: Seq[Int]) => {
      val list1 = List(seq1: _*)
      val list2 = List(seq2: _*)
      List.mergeLists(list1, list2) == List(seq1.zip(seq2).flatMap(x => Seq(x._1 + x._2)): _*)
    }
  }

  property("zipWith method") = forAll(genIntSeqs, arbitrary[Seq[Int]]) {
    (seq1: Seq[Int], seq2: Seq[Int]) => {
      val list1 = List(seq1: _*)
      val list2 = List(seq2: _*)
      val f: (Int, Int) => String = (x, y) => s"$x+$y"
      List.zipWith(list1, list2)(f) == List(seq1.zip(seq2).flatMap(e => Seq(f(e._1, e._2))): _*)
    }
  }

  property("startsWith method") = forAll(arbitrary[Seq[Int]], genIntSeqs) {
    (seq1: Seq[Int], seq2: Seq[Int]) => {
      val list1 = List(seq1: _*)
      val list2 = List(seq2: _*)
      List.startsWith(list1, list2) == seq1.startsWith(seq2)
    }
  }

  property("hasSubsequence") = forAll(arbitrary[Seq[Int]], genIntSeqs) {
    (seq1: Seq[Int], seq2: Seq[Int]) => {
      val list1 = List(seq1: _*)
      val list2 = List(seq2: _*)
      List.hasSubsequence(list1, list2) == seq1.containsSlice(seq2)
    }
  }
}
