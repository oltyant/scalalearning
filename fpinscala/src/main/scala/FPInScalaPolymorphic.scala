/**
 * Created by oltyant on 1/19/15.
 *
 * The code beneath is based on the Functional Programming in Scala 2nd chapter
 */
object FPInScalaParametricPolymorphism {
  /**
   * Find the first index of an explicit string in the given string array
   * If the string looked for is not present in the array then we will give back -1
   * @param arr - the input string array in where we try to find a given string
   * @param key - the given string what we try to find in the given array
   * @return - the first index of the string that we look for or -1
   */
  def findFirstInStringArray(arr: Array[String], key: String): Int = {
    @annotation.tailrec
    def go(i: Int): Int = {
      if (i >= arr.length) -1
      else if (arr(i) == key) i
      else go(i + 1)
    }
    
    go(0)
  }

  /**
   * Find the index of the first element in the array that fulfill the given function's criterion
   * @param arr - the given input array where we try to find the first element based on the given function
   * @param f - the given function that check whether an element in the array will be fulfill a criterion
   * @tparam A - the type parameter of the input array and function (could be anything)
   * @return - the first index of the element in the given array that gives back true when we apply f function on it or -1
   */
  def findFirst[A](arr: Array[A])(f: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= arr.length) -1
      else if (f(arr(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  /**
   * Check whether the given array is sorted or not
   * @param arr - the given array of tparam elements
   * @param ordered - a function that based on two neighbour elements give back true or false
   * @tparam A - the arr and ordered parameter type
   * @return - true if all the elements in the given array are fulfill the ordered function's criteria
   */
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= arr.length) true
      else if (!ordered(arr(n), arr(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }
  //example: val lessThan = new Function2[Int, Int, Boolean] {
  //  def apply(x: Int, y: Int): Boolean = x <= y
  //}
  //
  //isSorted(Array(-34,0,1,89), lessThan)
  //OR: isSorted(Array(-100,2,0,2,78,91), (x: Int, y: Int) => x <= y)
  //OR: isSorted[Int](Array(1, -1), _ <= _)

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)
  //example: val p = partial1(1, (a: Int, b: Double) => s"$a $b")
  //p(2.0)
  //res: String = "1 2.0"

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => ((b: B) => f(a, b))
  def curryShorterForm[A,B,C](f: (A, B) => C): A => (B => C) = a => f(a, _)
  //example: val c = curry(a: Int, (b: Double) => s"$a $b")
  //val c1 = c(1)
  //c1(2.0)
  //res: String = "1 2.0"

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => (f(a)(b))
  //example: val uc = uncurry((a: Int) => ((b: Double) => s"$a $b"))
  //uc(1, 2.0)
  //res: String = "1 2.0"

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
  //example: val c = compose[Int, Double, String](b => s"$b", a => a * 1.0)
  //c(30)
  //res: String = "30.0"

  /*
  Note that A,B,C could be any type including identical types eg: [Int, Int, Int] or [String, String, Int]
   */
}
