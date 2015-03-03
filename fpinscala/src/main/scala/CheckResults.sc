import MyModule._

def factorial(n: Int): Int = {
  def go(n: Int, acc: Int): Int = {
    if (n == 0) acc
    else go(n - 1, n * acc)
  }
  go(n, 1)
}

factorial(5)

def fib(n: Int): Int = {
  @annotation.tailrec
  def go(act: Int, acc2: Int, acc1: Int): Int = {
    if (act == 0) acc2
    else go(act - 1, acc1, acc2 + acc1)
  }

  go(n, 0, 1)
}

fib(11)

def findFirst[A](arr: Array[A])(f: A => Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int = {
    if (n >= arr.length) -1
    else if (f(arr(n))) n
    else loop(n + 1)
  }

  loop(0)
}

findFirst(Array(1,2,3,4,5))(_ % 2 == 0)
findFirst(Array("Jack", "The", "Rabbit"))(_ == "Rabbit")

def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if (n + 1 >= arr.length) true
    else if (!ordered(arr(n), arr(n + 1))) false
    else loop(n + 1)
  }
  loop(0)
}

isSorted(Array(-100,2,0,2,78,91), (x: Int, y: Int) => x <= y)
isSorted[Int](Array(1, -1), _ <= _)

val lessThan = new Function2[Int, Int, Boolean] {
  def apply(x: Int, y: Int): Boolean = x <= y
}

isSorted(Array(-34,0,1,89), lessThan)