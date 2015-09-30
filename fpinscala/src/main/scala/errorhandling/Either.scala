package errorhandling

import scala.{Either => _, None => _, Option => _, Some => _, Left => _, Right => _}

/**
 * Created by oltyant on 9/30/15.
 */
sealed trait Either [+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case err @ Left(e) => err
      //problem: harcd to test whether it is an exception with applying f as hre we have generics
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case r @ Right(v) => f(v)
    case l @ Left(e) => l
  }

  def orElse[EE >: E, B >: A](d: => Either[EE, B]): Either[EE, B] = this match {
    case err @ Left(e) => d
    case right @ Right(v) => right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap (x => b.map (y => f(x,y)))
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("Mean of an empty sequence")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = xs match {
    case Nil => Right(Nil)
    case h :: t => h.flatMap(x => sequence(t).map(x :: _))
  }

  def sequenceFold[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = xs.foldRight[Either[E, List[A]]](Right[List[A]](Nil))((a, acc) => a.map2(acc)(_ :: _))

  def tarverseFold[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right[List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequenceTricky[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = traverse(xs)(x => x)

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] = {
    if (name == null || name.isEmpty) Left("Name is empty or null")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age == null || age <= 0) Left("Age is not valid")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_,_))
  }
}
