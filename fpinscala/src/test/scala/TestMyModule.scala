import org.specs2.mutable.Specification

import scala.util.Random
import MyModule._

/**
 * Created by oltyant on 3/11/15.
 *
 * This test calss intend to cover the MyModule type
 */
object TestMyModule extends Specification {
  "The abs method" should {
    "result the input number wihout sign when the input number is negative" in {
      val negativeNumber = -2
      val result = abs(negativeNumber)
      result mustEqual 2
    }
    "result the input number when the input number is positive" in {
      val positiveNumber = 2
      val result = abs(positiveNumber)
      result mustEqual positiveNumber
    }
    "result the input number when the input number is zero" in {
      val zero = 0
      val result = abs(zero)
      result mustEqual zero
    }
  }

  "The factorial method" should {
    "Give back the proper one value when the input number is less than or equal one" in {
      val inputs = List(1, 0, new Random().nextInt(10) * -1)
      inputs.map(factorial).forall(_ mustEqual 1)
    }
    "Give back the proper factorial when the input number is greater than one" in {
      //ugly imperative definition of factorial computing
      //If it does not give back the same value that the tail recursive factorial definition in MyModule
      //then one of the definitions will be wrong
      def fact(n: Int): BigInt = {
        var f: BigInt = 1
        (1 to n).foreach(f *= _)
        f
      }
      val random = new Random().nextInt(100)
      val inputs = List(2, random).map(factorial)
      inputs.head mustEqual fact(2)
      inputs.last mustEqual fact(random)
    }
  }

  "The fib method" should {
    "Give back zero when the input number is less than or equal 0" in {
      val random = -(new Random().nextInt(100))
      fib(0) mustEqual 0
      fib(random) mustEqual 0
    }
    "Give back 1 when the input is either 1 or 2" in {
      fib(1) mustEqual 1
      fib(2) mustEqual 1
    }
    "Give back the proper number when the input is greater than 2" in {
      fib(3) mustEqual 2
      fib(30) mustEqual 832040
    }
  }
}
