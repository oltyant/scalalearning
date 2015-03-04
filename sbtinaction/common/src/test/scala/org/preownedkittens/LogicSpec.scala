package org.preownedkittens

import org.specs2.mutable.Specification

object LogicSpec extends Specification {
  "The matchLikeliHood method " should {
    "be 100% when all attributes match" in {
      val tabby = Kitten("1", Set("male", "tabby").toSeq)
      val prefs = UserPreferencies(Set("male", "tabby").toSeq)
      val result = Logic.matchLikeliHood(tabby, prefs)
      result must beGreaterThan(.99)
    }
    "be 0% when none of the attributes match" in {
      val plain = Kitten("2", Set("female", "plain").toSeq)
      val prefs = UserPreferencies(Set("male", "tabby").toSeq)
      val result = Logic.matchLikeliHood(plain, prefs)
      result must beLessThan(.001)
    }
    "be 66% when two from three attribts match" in {
      val tabby = Kitten("1", Set("female", "calico", "overweight").toSeq)
      val prefs = UserPreferencies(Set("female", "calico", "thin").toSeq)
      val result = Logic.matchLikeliHood(tabby, prefs)
      result must beBetween(0.666, 0.667)
    }
  }
}
