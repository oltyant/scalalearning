import org.scalatest.{GivenWhenThen, FlatSpec}
import FPInScalaParametricPolymorphism._

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
}
