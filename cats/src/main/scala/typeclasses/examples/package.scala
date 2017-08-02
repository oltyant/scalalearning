package typeclasses.examples

/**
  * Created by oltyan on 2017.07.16.
  */
package object printable {

  final case class Cat(name: String, age: Int, color: String)

  sealed trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit val personFormatter = new Printable[Person] {
      override def format(value: Person): String = s"${value.id} ${value.name} ${value.email}"
    }
    implicit val strFormatter = new Printable[String] {
      override def format(value: String) = value
    }
    implicit val intFormatter = new Printable[Int] {
      override def format(value: Int) = value.toString
    }
    implicit val catFormatter = new Printable[Cat] {
      override def format(value: Cat) = s"""${value.name} is a ${value.age} year old ${value.color} cat"""
    }
  }

  object Printable {
    def apply[A : Printable]: Printable[A] = implicitly[Printable[A]]
    def format[A : Printable](value: A): String = implicitly[Printable[A]].format(value)
    def print[A : Printable](value: A): Unit = println(format(value))
  }

  implicit class ImplicitPrinter[A : Printable](value: A) {
    def format = implicitly[Printable[A]].format(value)
    def print = println(format)
  }
}
