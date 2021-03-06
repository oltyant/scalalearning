package quickcheck

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

@RunWith(classOf[JUnitRunner])
class QuickCheckSuite extends FunSuite with Checkers {
  def propertiesToProp(properties: Properties) = Prop.all(properties.properties.map(_._2): _*)

  def checkBogus(p: Properties) {
    var ok = false
    try {
      check(propertiesToProp(p))
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }

  test("Binomial heap satisfies properties.") {
    check(propertiesToProp(new QuickCheckHeap with BinomialHeap))
  }

  test("Bogus (1) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with Bogus1BinomialHeap)
  }

  test("Bogus (2) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with Bogus2BinomialHeap)
  }

  test("Bogus (3) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with Bogus3BinomialHeap)
  }

  test("Bogus (4) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with Bogus4BinomialHeap)
  }

  test("Bogus (5) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with Bogus5BinomialHeap)
  }
}
