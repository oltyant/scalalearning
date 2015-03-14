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
   * example:
   * <code>
   * val lessThan = new Function2[Int, Int, Boolean] {
   *  def apply(x: Int, y: Int): Boolean = x <= y
   * }
   * </code>
   * <code>isSorted(Array(-34,0,1,89), lessThan)</code>
   * OR: <code>isSorted(Array(-100,2,0,2,78,91), (x: Int, y: Int) => x <= y)</code>
   * OR: <code>isSorted[Int](Array(1, -1), _ <= _)</code>
   *
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

  /**
   * The general definition of a partial function that has three parameter type.
   * It gives back a function that has one B type parameter and apply the f function on it.
   * The term partial here means that we have just one value parameter out of two
   * in this function so it just define the partially what we are able to do
   * example:
   * <code>val p = partial1(1, (a: Int, b: Double) => s"$a $b")<br/>
   * p(2.0)<br/>
   * res: String = "1 2.0"</code>
   *
   * @param a - the first value parameter that the given f function will use
   * @param f - the f function that apply on an element of type A and type B and give back type C
   * @tparam A - the first parameter type of the given f function
   * @tparam B - the second parameter type of the given f function
   * @tparam C - the result of the given f function
   * @return - a function that maps from B to C (must have parameter value B)
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a,b)

  /**
   * Implementation of the general currying that has some similarity with the above [[FPInScalaParametricPolymorphism.partial1]]
   * (check <a href="http://en.wikipedia.org/wiki/Currying" target="_blank">this</a> site for the basic concept)
   * example:
   * <code>
   *   val c = curry((a: Int, b: Double) => s"$a $b")
   *   val c1 = c(1)
   *   c1(2.0)
   *   res: String = "1 2.0"
   * </code>
   * <p>
   *   <b>Note:</b> that here the associativity matters so A => (B => C) is equivalent
   *   with A => B => C and with (A => B) => C
   * </p>
   *
   * @param f - a function that maps an A and B parameter value to a value of C type
   * @tparam A - the first parameter type of the f function and the result
   * @tparam B - the second parameter type of the f function and the result
   * @tparam C - the final result of the currying
   * @return - a function that expect a function with an input value of type A and gives back
   *         another function that expect a B typed value and eventually give back a C typed value
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => ((b: B) => f(a, b))

  /**
   * The same as [[FPInScalaParametricPolymorphism.curry]] but with shorter implementation
   *
   * @param f - a function that maps an A and B parameter value to a value of C type
   * @tparam A - the first parameter type of the f function and the result
   * @tparam B - the second parameter type of the f function and the result
   * @tparam C - the final result of the currying
   * @return - a function that expect a function with an input value of type A and gives back
   *         another function that expect a B typed value and eventually give back a C typed value
   */
  def curryShorterForm[A,B,C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  /**
   * The opposite of currying so the result and the parameters are the opposite.
   * It just apply the given function on all of the parameters passed into it
   * example:
   * <code>
   *   val uc = uncurry((a: Int) => ((b: Double) => s"$a $b"))
   *   uc(1, 2.0)
   *   res: String = "1 2.0"
   * </code>
   *
   * @param f - a function that maps A to B that maps to C
   * @tparam A - the first parameter of the given f function
   * @tparam B - the second parameter type of the f function
   * @tparam C - the result of the given f function
   * @return - a function that turn an A and a B to C
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => (f(a)(b))

  /**
   * The general definition of two functions composition.
   * In mathematics the f function composed g function (f o g)
   * means that we apply on the given parameter the g function and then we apply the given result the
   * f function.
   * example:
   * <code>
   * val c = compose[Int, Double, String](b => s"$b", a => a * 1.0)
   * c(30)
   * res: String = "30.0"
   * </code>
   *
   * @param f - maps the given type of B element to C
   * @param g - maps the given type of A element to B
   * @tparam A - the initial type of the computation
   * @tparam B - the subsequent type of the computation
   * @tparam C - the result type of the computation
   * @return - f(g(an element of type A)) gives back an element of type C
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  /*
  Note that A,B,C could be any type including identical types eg: [Int, Int, Int] or [String, String, Int]
   */
}
