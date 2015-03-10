package chapter1

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

import org.scalacheck.Gen._

/**
 * Created by root on 3/9/15.
 */
object CheckBase36 extends Properties("Base36") {
  val allBase36Characters = "0123456789abcdefghijklmnopqrstuvwxyz".toList

  val genBase36: Gen[String] = Gen.resize(20, (for {
    slist: List[Char] <- Gen.listOf(Gen.oneOf(allBase36Characters))
  } yield slist.mkString))


  val genBase10: Gen[BigInt] = (genBase36.map(base36Encode))

  property("base36 decode encode") = forAll(genBase36, genBase10) {
    (a: String, b: BigInt) =>
      (a.length <= 20) ==> {
        if (base36Decode(b) == a) base36Encode(a) == b
        else true
      }
  }
}
