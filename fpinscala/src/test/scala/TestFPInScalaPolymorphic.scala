import FPInScalaParametricPolymorphism._
import org.scalatest.{FlatSpec, GivenWhenThen}

/**
 * Created by oltyant on 3/13/15.
 */
class TestFPInScalaPolymorphic extends FlatSpec with GivenWhenThen {
  val arr = Array("zero","one","two","three","four")

  "The 'findFirstInStringArray' method" should "give back -1 when the searched string is not in the given array" in {
    val lookFor = "five"
    assert(-1 == findFirstInStringArray(arr, lookFor))
  }

  it should "give back the the proper index when we find the given string in the given array" in {
    Given("the searched strings one")
    val one = "one"
    And("two")
    val two = "two"
    And("three")
    val three = "three"
    When("the findFirstInStringArray is executed")
    Then("the proper 1")
    assert(1 == findFirstInStringArray(arr, one))
    And("the 2")
    assert(2 == findFirstInStringArray(arr, two))
    And("the 3 indices will be given back")
    assert(3 == findFirstInStringArray(arr, three))
  }

  "The 'findFirst' function" should "give back -1 when there is no element in the array that is fulfill the given criterion" in {
    Given("an array of Integers that does not contain the Int.MinValue")
    val arr = Array(2, -100, Int.MaxValue, -2341, 1, 5)
    And("a function that gives back true if an integer equal with Int.MinValue")
    val f: Int => Boolean = _ == Int.MinValue
    When("the findFirst method executed with the given array and function")
    val result = findFirst(arr)(f)
    Then("the result will be -1")
    assert(-1 == result)
  }

  it must "give back 1 when the fourth element of the given array is fulfill the criterion" in {
    Given("an array of Char lists where the fourth element's size greater than 5")
    val arr = Array(List('a','b'),List(),List('c','d','e'),List('1','2','3','4','5','6','7'))
    And("a function that gives back true if the lists size greater then 5")
    val f: List[Any] => Boolean = _.size > 5
    When("the findFirst method executed with the given array and function")
    val result = findFirst(arr)(f)
    Then("the result will be 4")
    assert(3 == result)
  }

  "The 'isSorted' function" must "give back true if the given array is empty" in {
    Given("an empty array of integers")
    val arr: Array[Int] = Array()
    And("an ordered function that check whether two integers are in desc order")
    val f = (x: Int, y: Int) => x >= y
    When("the isSorted function is executed with the given array and function")
    val result = isSorted(arr, f)
    Then("we have to get true")
    assert(result)
  }

  it must "give back false when we have an array where the elements are different" in {
    Given("an array of different strings")
    And("a function that checks whether all the elements are equal")
    val f = new Function2[String, String, Boolean] {
      def apply(first: String, second: String): Boolean = {
        first == second
      }
    }
    When("the isSorted function is executed with the given array and function")
    val result = isSorted(arr, f)
    Then("we have to get false")
    assert(!result)
  }

  it must "give back true when we have an array of Double where the elements are in ASC" in {
    Given("an array of Double ordered by ASC")
    val arr = Array(-1.0, 0.0, 0.0, 0.3, 1.5, 5.6)
    And("a function that checks whether all the elements are in order")
    val f = new Function2[Double, Double, Boolean] {
      def apply(first: Double, second: Double): Boolean = {
        first <= second
      }
    }
    When("the isSorted function is executed with the given array and function")
    val result = isSorted(arr, f)
    Then("we have to get false")
    assert(result)
  }

  "the 'partial1' function" must "print out an Int and a Double as a string" in {
    Given("the int 1025")
    val int = 1025
    And("the double 4.0")
    val d = 4.0
    And("a function that has int and double parameter and give back a string")
    val f = (a: Int, b: Double) => s"a: $a, b: $b"
    When("the 'partial1' executed with the given int and function")
    val part1 = partial1(int, f)
    And("its result called with the given double")
    val result = part1(d)
    Then("the result must be the string 'a: 1025, b: 4.0'")
    assert(result == "a: 1025, b: 4.0")
  }

  "the 'curry' function" should "chain functions in order to give back a comma separated string concatenation" in {
    Given("the int 1")
    val a = 1
    And("double 2.0")
    val d = 2.0
    And("function that computes a string with a comma separated int and double")
    val f: (Int, Double) => String = (x,y) => s"$x,$y"
    When("the 'curry' executed with the given function")
    val c1 = curry(f)
    And("if we pass into the result the given int and double")
    val result = c1(a)(d)
    Then("the result should be the string '1,2.0'")
    assert(result == "1,2.0")
  }

  "the 'curryShorterForm' function" should "chain functions in order to give back a comma separated string concatenation" in {
    Given("the int 1")
    val a = 1
    And("double 2.0")
    val d = 2.0
    And("function that computes a comma separated string of an int and a double")
    val f: (Int, Double) => String = (x,y) => s"$x,$y"
    When("the 'curryShorterForm' executed with the given function")
    val c1 = curryShorterForm(f)
    And("if we pass into the result the given int and double")
    val result = c1(a)(d)
    Then("the result should be the string '1,2.0'")
    assert(result == "1,2.0")
  }

  "the 'uncurry' function" should "compose chained function in order to give back a comma separated string concatenation" in {
    Given("the int 1")
    val a = 1
    And("double 2.0")
    val d = 2.0
    And("a chained function that computes a comma separated string of an int and a double")
    val f: (Int) => (Double) => String = (x) => (y) => s"$x,$y"
    When("the 'curryShorterForm' executed with the given function")
    val c1 = uncurry(f)
    And("if we pass into the result the given int and double")
    val result = c1(a, d)
    Then("the result should be the string '1,2.0'")
    assert(result == "1,2.0")
  }

  "the 'compose' function" should "compose two given functions that convert from int to double and double to string respectively" in {
    Given("the int 1")
    val a = 1
    And("a function that maps an int to a double")
    val g: (Int) => (Double) = _.toDouble
    And("a function that maps a double to a string")
    val f: (Double) => (String) = _.toString
    When("the 'compose' executed with the given functions")
    val comp = compose(f, g)
    And("the resulted function applied on the given int")
    val result = comp(a)
    Then("the result should be the string '1.0'")
    assert(result == "1.0")
  }
}
