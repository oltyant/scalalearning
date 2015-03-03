package practice.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(root: Tree[Int]): Int = root match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](root: Tree[A]): Int = root match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](root: Tree[A]): Int = fold(root)((a) => 1)((a, b) => 1 + a + b)

  def maxViaFold(root: Tree[Int]): Int = fold(root)(a => a)(_ max _)

  def depthViaFold[A](root: Tree[A]): Int = fold(root)(a => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](root: Tree[A], f: A => B): Tree[B] = fold(root)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}
