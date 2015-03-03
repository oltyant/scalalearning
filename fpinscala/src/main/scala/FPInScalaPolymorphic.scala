/**
 * Created by root on 1/19/15.
 */
object FPInScalaParametricPolymorphism {
  def findFirstInStringArray(arr: Array[String], key: String): Int = {
    @annotation.tailrec
    def go(i: Int): Int = i match {
      case _ if i >= arr.length => -1
      case _ if arr(i) == key => i
      case _ => go(i - 1)
    }
    
    go(0)
  }

  def findFirst[A](arr: Array[A])(f: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= arr.length) -1
      else if (f(arr(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

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
  //or: isSorted[Int](Array(1, -1), _ <= _)

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
