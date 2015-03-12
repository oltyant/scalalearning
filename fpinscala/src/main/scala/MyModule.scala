/**
 * Created by oltyan on 1/18/15.
 *
 * The below code is based on the Functional Programming in Scala book's 2nd chapter
 */
object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): BigInt = {
    @annotation.tailrec
    def go(n: Int, acc: BigInt): BigInt = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  /**
   * Calculate the Nth Fibonacci number
   * @param n - the input of the calculation that is the Nth number
   * @return - The calculated Fibonacci number
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(act: Int, acc2: Int, acc1: Int): Int = act match {
      case num: Int if num <= 0  => acc2
      case _ =>  go(act - 1, acc1, acc2 + acc1)
    }

    go(n, 0, 1)
  }

  private def formatFunction(n: Int, name: String)(f: Int => BigInt): String = s"The $name value of $n is ${f(n)}"

  def main(args: String*) = println(formatFunction(-42, "factorial")(factorial))
}
