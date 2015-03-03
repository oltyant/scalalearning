import org.specs2.mutable.Specification

object LogicSpec extends Specification {
  "The matchLikeliHood method " should {
    "be 100% when all attributes match" in {
      val tabby = Kitten("1", List("male", "tabby"))
      val prefs = UserPreferencies(List("male", "tabby"))
      val result = Logic.matchLikeliHood(tabby, prefs)
      result must beGreaterThan(.99)
    }
    "be 0% when none of the attributes match" in {
      val plain = Kitten("2", List("female", "plain"))
      val prefs = UserPreferencies(List("male", "tabby"))
      val result = Logic.matchLikeliHood(plain, prefs)
      result must beLessThan(.001)
    }
  }
}
