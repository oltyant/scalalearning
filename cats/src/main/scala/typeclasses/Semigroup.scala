package typeclasses

/**
  * Created by oltyan on 2017.07.29..
  */
trait Semigroup[A] {
  def combine(a: A, aa: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A : Monoid] = implicitly[Monoid[A]]
}

case class Order(totalCost: Int, quantity: Int)

object MonoidImplicits {
  implicit val stringMonoid = new Monoid[String] {
    override def combine(a: String, aa: String): String = s"""$a$aa"""
    override def empty: String = ""
  }

  implicit val orderMonoid = new Monoid[Order] {
    override def combine(a: Order, aa: Order) = Order(a.totalCost + aa.totalCost, a.quantity + aa.quantity)

    override def empty = Order(0, 0)
  }

  implicit def setMonoid[A] = new Monoid[Set[A]] {
    override def combine(a: Set[A], b: Set[A]): Set[A] = a union b
    override def empty = Set.empty[A]
  }
  //How to use
  implicit def optionMonoid[A : Monoid] = new Monoid[Option[A]] {
    override def combine(a: Option[A], aa: Option[A]): Option[A] = a.flatMap(x => aa.map(y => x.combine(y)))
    override def empty = None
  }

  val example = setMonoid[Int].combine(Set(1,2), Set(3,4))

  implicit class MonoidOps[A : Monoid](value: A) {
    def combine(other: A): A = Monoid[A].combine(value, other)
    def |+|(other: A): A = combine(other)
  }

  val example2 = "" |+| ""
  val example3 = Set(1, 2) |+| Set(3, 4)
  val example4 = Monoid[Option[String]].combine(Some(""), Some(""))
  //cannot do Some("").combine(Some("")) as it is not an Option

  def adder(items: List[Option[Int]]): Option[Int] = items.reduceLeft {
    (acc, next) => for {
      x <- acc
      y <- next
    } yield x + y
  }

  def addAll(items: List[Order]): Order = items.foldLeft(Monoid[Order].empty)(_ |+| _)
}
