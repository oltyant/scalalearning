package practice.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, t) => Cons(e, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(sl: List[A], i: Int): List[A] = sl match {
      case Cons(h, t) if i > 0 => loop(t, i - 1)
      case _ => sl
    }
    loop(l, n)
  }

  @annotation.tailrec
  def dropElegant[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n > 0 => dropElegant(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(sl: List[A]): List[A] = sl match {
      case Cons(h, t) if f(h) => loop(t)
      case _ => sl
    }
    loop(l)
  }

  @annotation.tailrec
  def dropWhileElegant[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhileElegant(t)(f)
    case _ => l
  }

  def add[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, add(t, l2))
  }

  //It is not tailrec :(
  def init[A](l: List[A]): List[A] = {
    def loop(sl: List[A]): List[A] = sl match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, loop(t))
    }
    loop(l)
  }

  //sum and product do the same if we remove the products middle case so we can generalize that
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)

  def product2(l: List[Double]) = foldRight(l, 1.0)((x, y) => x * y)

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(ls: List[A], acc: B): B = ls match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(l, z)
  }

  @annotation.tailrec
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft2(t, f(z, h))(f)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((x,y) => f(y,x))

  def append[A](as: List[A], t: A): List[A] = foldRight2(as, Cons(t, Nil: List[A]))((x,y) => Cons(x,y))

  def flatten[A](ll: List[List[A]]): List[A] = foldLeft2(ll, Nil: List[A])(add)

  def add1(ls: List[Int]): List[Int] = foldRight2(ls, Nil:List[Int])((a,b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] = {
    @annotation.tailrec
    def loop(a: List[Double], acc: List[String]): List[String] = a match {
      case Nil => Nil
      case Cons(h, t) => loop(t, Cons(h.toString, acc))
    }
    loop(l, Nil)
  }

  def doubleToStringFold(l: List[Double]): List[String] = foldRight2(l, Nil:List[String])((a,b) => Cons(a.toString, b))

  def map[A, B](l: List[A], f: A => B): List[B] = foldRight2(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])( f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((a, b) => if (f(a)) Cons[A](a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight2(as, Nil: List[B])((a: A, b: List[B]) => add(f(a), b))

  def filterFM[A](as: List[A])( f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else List())

  def mergeLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, mergeLists(t, t2))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def loop[A](orig: List[A], s: List[A], acc: Boolean): Boolean = (orig, s) match {
      case (Cons(h, t), Cons(hs, Nil)) if (h == hs) => true
      case (Cons(h, t), Cons(hs, ts)) if (h == hs) => loop(t, ts, true)
      case (Cons(h, t), Cons(hs, ts)) if (h != hs) => if (acc) false else loop(t, Cons(hs, ts), false)
      case _ => false
    }
    loop(sup, sub, false)
  }
}
