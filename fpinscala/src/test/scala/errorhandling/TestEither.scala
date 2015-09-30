package errorhandling

import scala.{Either => _, None => _, Option => _, Some => _, Left => _, Right => _}

import org.scalatest.{GivenWhenThen, FlatSpec}

/**
 * Created by root on 9/30/15.
 */
class TestEither extends FlatSpec with GivenWhenThen {
  "The 'map' method" should "give Some if we use valid input function" in {
    Given("the input Right 1")
    val op = Right("1")
    When("we map it with toInt function")
    val res = op.map(_.toInt)
    Then("It should give back Right(1)")
    assert(res == Right(1))
  }

  "The 'flatMap' method" should "give Right if we use valid input function" in {
    Given("the input Right(1)")
    val op: Either[Exception, Int] = Right(1)
    When("we flatmap it with times two")
    val res: Either[Exception, Int] = op.flatMap((x : Int) => Right(x * 2))
    Then("It should give back Right(2)")
    assert(res == Right(2))
  }

  "The 'flatMap' method" should "give Left(e) if we use invalid input function parameter" in {
    Given("the input Right(1)")
    val op: Either[Exception, Int] = Right(1)
    When("we flatmap it with divide by zero")
    val res: Either[Exception, Int] = op.flatMap((x : Int) => Either.Try(x / 0))
    Then("It should give back Left")
    assert(res.isInstanceOf[Left[Exception]])
  }

  "The 'orElse' method" should "give Right if we use valid input function" in {
    Given("the input Right(1)")
    val op: Either[Exception, Int] = Right(1)
    When("we flatmap it with times two and orElse None")
    val res = op.flatMap((x : Int) => Right(x * 2)).orElse(Left("exception raised"))
    Then("It should give back 2")
    assert(res == Right(2))
  }

  "The 'orElse' method" should "give the default value Right(-1) if we use invalid input function parameter" in {
    Given("the input Right(1)")
    val op: Either[Exception, Int] = Right(1)
    When("we flatmap it with divide by zero and orElse Right(-1)")
    val res = op.flatMap((x : Int) => try { Right(x / 0) } catch { case e: Exception => Left(e)}).orElse(Right(-1))
    Then("It should give back Right(-1)")
    assert(res == Right(-1))
  }

  "The 'map2' method" should "produce a Right(String) from a Right(Int) and a Right(Double)" in {
    Given("the input Right(12)")
    val a = Right(12)
    And("Some(7.0)")
    val b = Right(7.0)
    And("we have a function that creates a string from an int and a double")
    val f: (Int, Double) => String = (a, b) => s"a: $a, b: $b"
    When("we apply map2 on the inputs with this function")
    val res = a.map2(b)(f)
    Then("The result must be Right('a: 12, b: 7.0')")
    assert(res == Right("a: 12, b: 7.0"))
  }

  "The 'map2' method" should "produce a Left when one of the inputs are Left" in {
    Given("the input Right(12)")
    val a = Right(12)
    And("Left(Exception)")
    val b: Either[Exception, Double] = Either.Try(1/0)
    And("we have a function that creates a string from an int and a double")
    val f: (Int, Double) => String = (a, b) => s"a: $a, b: $b"
    When("we apply map2 on the inputs with this function")
    val res = a.map2(b)(f)
    Then("The result must be Left")
    assert(res.isInstanceOf[Left[Exception]])
  }

  "The 'sequence' method" should "produce Right(List) from an input List of Either where only Right elements have" in {
    Given("the input List(Right(12),Right(1),Right(-1),Right(-2))")
    val as = List(Right(12),Right(1),Right(-1),Right(-2))
    When("we call 'sequence' with the input param")
    val res = Either.sequence(as)
    Then("The result must be Right(List(12,1,-1,-2))")
    assert(res == Right(List(12,1,-1,-2)))
  }

  "The 'sequence' method" should "produce Left from an input List of Either where there is a Left element" in {
    Given("the input List(Right(12),Left('error!'),Right(-1),Right(-2))")
    val as = List(Right(12),Left("error!"),Right(-1),Right(-2))
    When("we call 'sequence' with the input param")
    val res = Either.sequence(as)
    Then("The result must be Left")
    assert(res == Left("error!"))
  }

  "The 'traverse' method" should "produce Right(List) from an input List" in {
    Given("the input List('12','1','-1','-2)'")
    val as = List("12","1","-1","-2")
    And("a function which map String into Int")
    val f: String => Either[Exception, Int] = x => try { Right(x.toInt) } catch {case e: Exception => Left(e)}
    When("we call 'traverse' with the input list")
    val res = Either.traverse[Exception, String, Int](as)(f)
    Then("The result must be Right(List(12,1,-1,-2))")
    assert(res == Right(List(12,1,-1,-2)))
  }

  "The 'traverse' method" should "produce Left from an input List" in {
    Given("the input List('12','1a','-1','-2)'")
    val as = List("12","1a","-1","-2")
    And("a function which map String into Int")
    val f: String => Either[Exception, Int] = x => try { Right(x.toInt) } catch {case e: Exception => Left(e)}
    When("we call 'traverse' with the input list")
    val res = Either.traverse[Exception, String, Int](as)(f)
    Then("The result must be Left")
    assert(res.isInstanceOf[Left[Exception]])
  }
}

