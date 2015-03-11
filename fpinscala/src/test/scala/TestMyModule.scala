import org.specs2.mutable.Specification

/**
 * Created by oltyan on 3/11/15.
 *
 * This test calss intend to cover the MyModule type
 */
object TestMyModule extends Specification {
  "The abs method" should {
    "result the input number wihout sign when the input number is negative" in {
      val negativeNumber = -2
      val module = MyModule
      val result = module.abs(negativeNumber)
      result mustEqual 2
    }
    "result the input number when the input number is positive" in {
      val positiveNumber = 2
      val module = MyModule
      val result = module.abs(positiveNumber)
      result mustEqual positiveNumber
    }
    "result the input number when the input number is zero" in {
      val zero = 0
      val module = MyModule
      val result = module.abs(zero)
      result mustEqual zero
    }
  }
}
