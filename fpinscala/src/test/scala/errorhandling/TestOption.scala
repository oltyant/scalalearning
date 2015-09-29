package errorhandling

import org.scalatest.{FlatSpec, GivenWhenThen}

import _root_.scala.{Either => _, None => _, Option => _, Some => _}

/**
 * Created by root on 9/29/15.
 */
class TestOption extends FlatSpec with GivenWhenThen {
  "The 'map' method" should "give Some if we use valid input function" in {
    Given("the input Option 1")
    val op = Some("1")
    When("we map it with toInt function")
    val res = op.map(_.toInt)
    Then("It should give back Some(1)")
    assert(res == Some(1))
  }

  "The 'map' method" should "give None if we use invalid input function parameter" in {
    Given("the input Option one")
    val op = Some("one")
    When("we map it with toInt function")
    val res = op.map((x: String) => try { Some(x.toInt) } catch {case _ => None}).getOrElse(None)
    Then("It should give back None")
    assert(res == None)
  }

  "The 'flatMap' method" should "give Some if we use valid input function" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with times two")
    val res: Option[Int] = op.flatMap((x : Int) => Some(x * 2))
    Then("It should give back Some(2)")
    assert(res == Some(2))
  }

  "The 'flatMap' method" should "give None if we use invalid input function parameter" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with divide by zero")
    val res: Option[Int] = op.flatMap((x : Int) => try { Some(x / 0) } catch { case _ => None})
    Then("It should give back None")
    assert(res == None)
  }

  "The 'getOrElse' method" should "give an expected Int if we use valid input function" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with times two and getOrElse 0")
    val res = op.flatMap((x : Int) => Some(x * 2)).getOrElse(0)
    Then("It should give back 2")
    assert(res == 2)
  }

  "The 'getOrElse' method" should "give the default value 0 if we use invalid input function parameter" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with divide by zero")
    val res = op.flatMap((x : Int) => try { Some(x / 0) } catch { case _ => None}).getOrElse(0)
    Then("It should give back 0")
    assert(res == 0)
  }

  "The 'orElse' method" should "give Some if we use valid input function" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with times two and orElse None")
    val res = op.flatMap((x : Int) => Some(x * 2)).orElse(None)
    Then("It should give back 2")
    assert(res == Some(2))
  }

  "The 'orElse' method" should "give the default value Some(-1) if we use invalid input function parameter" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we flatmap it with divide by zero and orElse Some(-1)")
    val res = op.flatMap((x : Int) => try { Some(x / 0) } catch { case _ => None}).orElse(Some(-1))
    Then("It should give back Some(-1)")
    assert(res == Some(-1))
  }

  "The 'filter' method" should "give back the Some if the criterion fulfilled" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we filter it with % 2 == 1")
    val res = op.filter((x : Int) => x % 2 == 1)
    Then("It should give back Some(1)")
    assert(res == Some(1))
  }

  "The 'filter' method" should "give None if we use a criterion which is not fulfilled" in {
    Given("the input Some(1)")
    val op: Option[Int] = Some(1)
    When("we filter it with % 2 == 0")
    val res = op.filter((x : Int) => x % 2 == 0)
    Then("It should give back None")
    assert(res == None)
  }

  "The 'variance' method" should "give 4.0 when the input is List(-2.0, 2.0)" in {
    Given("the input List(-2.0, 2.0)")
    val xs: List[Double] = List(-2.0, 2.0)
    When("we apply variance on it")
    val res = Option.variance(xs)
    Then("It should give back Some(4.0)")
    assert(res == Some(4.0))
  }

  "The 'variance' method" should "be produce the same result as 'variance0'" in {
    Given("the input Seq(2.0, 3.5, 6.7, 8.9, 2.1, -0.23)")
    val xs = Seq(2.0, 3.5, 6.7, 8.9, 2.1, -0.23)
    When("we apply on it variance")
    val res1 = Option.variance(xs)
    And("apply on it variance0")
    val res2 = Option.variance0(xs)
    Then("The two results should be the same")
    assert(res1 == res2)
  }

  "The 'map2' method" should "produce a Some(String) from an Some(Int) and a Some(Double)" in {
    Given("the input Some(12)")
    val a = Some(12)
    And("and Some(7.0)")
    val b = Some(7.0)
    And("we have a function that creates a string from an int and a double")
    val f: (Int, Double) => String = (a, b) => s"a: $a, b: $b"
    When("we apply map2 on the inputs with this function")
    val res = Option.map2(a, b)(f)
    Then("The result must be Some('a: 12, b: 7.0')")
    assert(res == Some("a: 12, b: 7.0"))
  }

  "The 'map2' method" should "produce a None when one of the inputs are None" in {
    Given("the input Some(12)")
    val a = Some(12)
    And("and None")
    val b: Option[Double] = None
    And("we have a function that creates a string from an int and a double")
    val f: (Int, Double) => String = (a, b) => s"a: $a, b: $b"
    When("we apply map2 on the inputs with this function")
    val res = Option.map2(a, b)(f)
    Then("The result must be None")
    assert(res == None)
  }

  "The 'sequence' method" should "produce Some(List) from an input List of Option where only Some elements have" in {
    Given("the input List(Some(12),Some(1),Some(-1),Some(-2))")
    val as = List(Some(12),Some(1),Some(-1),Some(-2))
    When("we call 'sequence' with the input param")
    val res = Option.sequence(as)
    Then("The result must be ome(List(12,1,-1,-2))")
    assert(res == Some(List(12,1,-1,-2)))
  }

  "The 'sequence' method" should "produce None from an input List of Option where there is a None element" in {
    Given("the input List(Some(12),None,Some(-1),Some(-2))")
    val as = List(Some(12),None,Some(-1),Some(-2))
    When("we call 'sequence' with the input param")
    val res = Option.sequence(as)
    Then("The result must be None")
    assert(res == None)
  }

  "The 'traverse' method" should "produce Some(List) from an input List" in {
    Given("the input List('12','1','-1','-2)'")
    val as = List("12","1","-1","-2")
    And("a function which map String into Int")
    val f: String => Option[Int] = x => try { Some(x.toInt) } catch {case _ => None}
    When("we call 'traverse' with the input list")
    val res = Option.traverse[String, Int](as)(f)
    Then("The result must be Some(List(12,1,-1,-2))")
    assert(res == Some(List(12,1,-1,-2)))
  }

  "The 'traverse' method" should "produce None from an input List" in {
    Given("the input List('12','1a','-1','-2)'")
    val as = List("12","1a","-1","-2")
    And("a function which map String into Int")
    val f: String => Option[Int] = x => try { Some(x.toInt) } catch {case _ => None}
    When("we call 'traverse' with the input list")
    val res = Option.traverse[String, Int](as)(f)
    Then("The result must be None")
    assert(res == None)
  }
}
