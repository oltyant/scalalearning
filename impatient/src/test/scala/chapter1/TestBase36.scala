package chapter1

import org.scalatest.FlatSpec

class TestBase36 extends FlatSpec {
  "A 'random' String" should "created from a BigInt with Base36 decoding" in {
    val source: BigInt = BigInt("9949944518853315416774479073037")
    val base36: String = base36Decode(source)
    assert("qsnvbevtomcj38o06kul" == base36)
  }
  
  it should "be decoded back to the source BigInt" in {
    val source: BigInt = BigInt("3124123452345634634634565634")
    val base36: String = base36Decode(source)
    assert(base36Encode(base36) == source)
  }
}
