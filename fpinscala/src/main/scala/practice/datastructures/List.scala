package practice.datastructures

/*
 * Based on the book's 3rd chapter
 */

/**
 * The trait of the functional immutable List data structure
 * @tparam A - A List can contain any type and its descendants
 */
sealed trait List[+A]

/**
 * The Empty List
 */
case object Nil extends List[Nothing]

/**
 * The non-empty List that contain at least one element, namely the head
 * @param h - the head of the list
 * @param t - the tail of the List which is another List
 * @tparam A - A List can contain any type and its descendants
 */
case class Cons[+A](h: A, t: List[A]) extends List[A]

/**
 * The companion object of the immutable List class
 */
object List {
  /**
   * Sum all of the elements in the given list
   * @param l - the given list that will be reduced to an Int
   * @return - the sum of the given list's elements
   */
  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  /**
   * Product of all the elements in the given list
   * @param l - the given list that will be reduced to a Double
   * @return - the product of the given list's elements
   */
  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  /**
   * Syntactic sugar to construct a List of A from a varargs of A
   *
   * @param as - the given varags of A
   * @tparam A - the parametric type of the given varargs and the result
   * @return - a list of A
   */
   //commented out as we need a tail recursive version
  /*def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else apply(Nil: List[A], as.toSeq.reverse: _*)

  /**
   * Advanced: Syntactic sugar to construct a List of A from a varargs of A
   * (it is <b>tailrec optimized</b>)
   *
   * @param as - the given varags of A
   * @tparam A - the parametric type of the given varargs and the result
   * @return - a list of A
   */
  @annotation.tailrec
  private def apply[A](acc: List[A], as: A*): List[A] =
    if (as.isEmpty) acc
    else apply(Cons(as.head, acc), as.tail: _*)

  /**
   * Give back the tail of the given list (drop the head of the list)
   *
   * @param l - the given list of A
   * @tparam A - the parametric type of the input and output
   * @return - the given list's tail
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  /**
   * Change the head of the given list to the given element
   *
   * @param l - the given list of A
   * @param e - the element that will go to the given's list head
   * @tparam A - parametric type of the in/output list
   * @return - list with the new head
   */
  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, t) => Cons(e, t)
  }

  /**
   * Drop n element from the beginning of a given list
   *
   * @param l - the given list of elements A
   * @param n - the first n number
   * @tparam A - type parameter of the lists
   * @return - a list that does not contain the first n element of the given list
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(sl: List[A], i: Int): List[A] = sl match {
      case Cons(h, t) if i > 0 => loop(t, i - 1)
      case _ => sl
    }
    loop(l, n)
  }

  /**
   * Drop n element from the beginning of a given list
   *
   * @param l - the given list of elements A
   * @param n - the first n number
   * @tparam A - type parameter of the lists
   * @return - a list that does not contain the first n element of the given list
   */
  @annotation.tailrec
  def dropElegant[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n > 0 => dropElegant(t, n - 1)
    case _ => l
  }

  /**
   * Drop the elements from the beginning of a given list while they fulfill the given criterion
   *
   * <pre>{@code
   * List.dropWhile(List(1,2,3,-2,4,0), _ < 3) == List(3,-2,4,0)
   * }
   * </pre>
   *
   * @param l - the given list of elements A
   * @param f - the criterion function
   * @tparam A - type parameter of the lists and the given criterion
   * @return - a list that contains the subsequence of the given list.<br/>
   * The first element of this result list is the first element of the given list which is not fulfill the given criterion  
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(sl: List[A]): List[A] = sl match {
      case Cons(h, t) if f(h) => loop(t)
      case _ => sl
    }
    loop(l)
  }

  /**
   * Drop the elements from the beginning of a given list while they fulfill the given criterion
   *
   * <pre>{@code
   * List.dropWhile(List(1,2,3,-2,4,0), _ < 3) == List(3,-2,4,0)
   * }
   * </pre>
   *
   * @param l - the given list of elements A
   * @param f - the criterion function
   * @tparam A - type parameter of the lists and the given criterion
   * @return - a list that contains the subsequence of the given list.<br/>
   * The first element of this result list is the first element of the given list which is not fulfill the given criterion  
   */
  @annotation.tailrec
  def dropWhileElegant[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhileElegant(t)(f)
    case _ => l
  }

  /**
   * Concatenate the two given lists withsame parameter type in the order they stands in the parameter list
   * 
   * @param l1 - the first given list of A
   * @param l2 - the second given list of A
   * @tparam A - the elements type that the given lists contain
   * @return - a list that contains the given list's elements in the same order they appeared in the lists respectively
   * and in the order they passed into this function
   */
  def add[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, add(t, l2))
  }

  /**
   * Remove the last element from the given list
   * 
   * @param l - the given list of A
   * @tparam A - the elements type that the given list contains
   * @return - a list that contains the given list's elements except the last one
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   * General solution for fold a list from left to right.
   * This direction means that we start from the head element and incrementally
   * apply the given function on them and on the remaining elements. During the apply we reduce the list
   * and accumulate the result of the current step.
   *
   * @param as - input list of A
   * @param z - the zero (init) element of the fold operation. It acts as an accumulator
   * @param f - the function that gets the actual element of the input list and the remaining elements applied
   * @tparam A - the elements type in the given list
   * @tparam B - the zero element's and the result type
   * @return - the result when we apply the f function on all of the elements and reduce the list from left to right
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  /**
   * Sum the given list's elements with the use of foldRight function
   * 
   * @param l - the given list of Ints
   * @return - the sum of the elements of the list
   */
  def sum2(l: List[Int]) = foldRight(l, 0)((x, y) => x + y)

  /**
   * Make the product of the given list's elements with the use of foldRight function
   * 
   * @param l - the given list of Doubles
   * @return - the product of the elements of the list
   */
  def product2(l: List[Double]) = foldRight(l, 1.0)((x, y) => x * y)

  /**
   * Compute the length of the given list with the use of foldRight
   *
   * @param l - list of A elements
   * @tparam A - the parametric type of the given list
   * @return - the length of the given list
   **/
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  /**
   * General solution for fold a list from right to left (optimized for tail recursion).
   * This direction means that we start from the last element and incrementally
   * apply the given function on them and on the previous elements. During the apply we reduce the list
   * and accumulate the result of the current step.
   *
   * @param as - input list of A
   * @param z - the zero (init) element of the fold operation. It acts as an accumulator
   * @param f - the function that gets the actual element of the input list and the remaining elements applied
   * @tparam A - the elements type in the given list
   * @tparam B - the zero element's and the result type
   * @return - the result when we apply the f function on all of the elements and reduce the list from right to left
   */
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(ls: List[A], acc: B): B = ls match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(l, z)
  }

  /**
   * General solution for fold a list from right to left without nested function (optimized for tail recursion).
   * This direction means that we start from the last element and incrementally
   * apply the given function on them and on the previous elements. During the apply we reduce the list
   * and accumulate the result of the current step.
   *
   * @param as - input list of A
   * @param z - the zero (init) element of the fold operation. It acts as an accumulator
   * @param f - the function that gets the actual element of the input list and the remaining elements applied
   * @tparam A - the elements type in the given list
   * @tparam B - the zero element's and the result type
   * @return - the result when we apply the f function on all of the elements and reduce the list from right to left
   */
  @annotation.tailrec
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft2(t, f(z, h))(f)
  }

  /**
   * Sum the given list's elements with the use of foldLeft function
   * 
   * @param l - the given list of Ints
   * @return - the sum of the elements of the list
   */
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  /**
   * Make the product of the given list's elements with the use of foldLeft function
   * 
   * @param l - the given list of Ints
   * @return - the product of the elements of the list
   */
  def product3(l: List[Int]) = foldLeft(l, 1)(_ * _)

  /**
   * Reverse the given list's elements with the use of foldLeft function
   * 
   * @param l - the given list of Ints
   * @tparam A - the parametric type of the given list
   * @return - the reverse of the given list
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) => Cons(y, x))

  /**
   * The tail recursive solution for fold a list from left to right (rely on reverse and foldLeft functions).
   * This direction means that we start from the head element and incrementally
   * apply the given function on them and on the remaining elements. During the apply we reduce the list
   * and accumulate the result of the current step.
   *
   * @param as - input list of A
   * @param z - the zero (init) element of the fold operation. It acts as an accumulator
   * @param f - the function that gets the actual element of the input list and the remaining elements applied
   * @tparam A - the elements type in the given list
   * @tparam B - the zero element's and the result type
   * @return - the result when we apply the f function on all of the elements and reduce the list from left to right
   */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((x,y) => f(y,x))

  /**
   * Append an element to an end of a given list
   *
   * @param as - the input list of A elements
   * @param t - the element that need to be inserted to the end of the given list
   * @tparam - A the parametric type of the given list
   * @return - the given list with the appended value at its end 
   */
  def append[A](as: List[A], t: A): List[A] = foldRight2(as, Cons(t, Nil: List[A]))((x,y) => Cons(x,y))

  /**
   * Make List of A from the list of the list of A's where the result contain all the A elements (redundantly)
   * that can be found in the given list
   *
   * @param ll - the given list of list of A elements
   * @tparam A - the elements that the result list contains
   * @return - the list that contains all the A's that the list of list of A input list contain
   */
  def flatten[A](ll: List[List[A]]): List[A] = foldLeft2(ll, Nil: List[A])(add)

  /**
   * Increment the integers in the given list and construct
   * a new list with the tail recursive version of fold right
   *
   * Example: List.add1(List(0,1,2,3)) == List(1,2,3,4)
   *
   * @param ls - the given list of integers
   * @return - the list of integers incremented with one
   */
  def add1(ls: List[Int]): List[Int] = foldRight2(ls, Nil:List[Int])((a,b) => Cons(a + 1, b))

  /**
   * Map a list of doubles to list of strings. Literally this means it calls the toString
   * method on each of the elements and construct a new list (not tail recursive)
   *
   * @param l - the given list of doubles
   * @return - the list of strings based on the given list of integers
   */
  def doubleToString(l: List[Double]): List[String] = {
    def loop(a: List[Double], acc: List[String]): List[String] = a match {
      case Nil => acc
      case Cons(h, t) => Cons(h.toString(), loop(t, acc))
    }
    loop(l, Nil)
  }

  /**
   * Map a list of doubles to list of strings. Literally this means it calls the toString
   * method on each of the elements and construct a new list (uses fold right)
   *
   * @param l - the given list of doubles
   * @return - the list of strings based on the given list of integers
   */
  def doubleToStringFold(l: List[Double]): List[String] = foldRight2(l, Nil:List[String])((a,b) => Cons(a.toString, b))

  /**
   * Map a list of A's to list of B's. Literally this means it calls the given f function
   * on each of the elements and construct a new list
   *
   * @param l - the given list of A's
   * @param f - the transformer function (map an A element to B)
   * @return - the list of B's based on the given list of A's
   */
  def map[A, B](l: List[A], f: A => B): List[B] = foldRight2(l, Nil: List[B])((a, b) => Cons(f(a), b))

  /**
   * Filter the given list's elements based on the given criterion
   *
   * @param as - the given list of A's
   * @param f - the criterion which map A to Boolean
   * @tparam A - the parameter type of the given list and the result list
   * @return - all the elements in a new list those fulfill the criterion
   */
  def filter[A](as: List[A])( f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((a, b) => if (f(a)) Cons[A](a, b) else b)

  /**
   * A flatMap that apply the given f transformer function on all of the elements contain the given list
   *
   * @param as - the given list of A's
   * @param f - the tranformer function that map A element to List[B]
   * @tparam A - the input list's parameter type
   * @tparam B - the result list's parameter type
   * @return - the new list that contains all the elements that the input list but with List[B]
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight2(as, Nil: List[B])((a: A, b: List[B]) => add(f(a), b))

  /**
   * A filtering based on a flatMap rather than a map
   *
   * @param as - the given list
   * @param f - the filter function that map an A element to Boolean
   * @tparam A - the parameter type of the given list
   * @return - the filtered list
   */
  def filterFM[A](as: List[A])( f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else List())

  /**
   * Merge two list of integers. Sum the elements in the first list with the second list corresponding element.
   * If the input lists' length are not the same then just the corresponding elements summed all the others
   * can be considered as a removed element
   *
   * @param as - the first list of integers
   * @param bs - the second list of integers
   * @return - the merged list of integers
   */
  def mergeLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, mergeLists(t, t2))
  }

  /**
   * Merge the first list of A's with the second list of B's in order so in the end we have a list of C
   *
   * @param as - the list of A's
   * @param bs - the list of B's
   * @param f - the transform function which map from (A, B) pair to C
   * @tparam A - the first input list's parameter type
   * @tparam B - the second input list's parameter type
   * @tparam C - the result list's parameter type
   * @return - the merged list of C's
   */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
  }

  /**
   * Check whether the given list of A starts with the given prefix (also a list of A's)
   *
   * @param l - the input list of A's
   * @param prefix - the prefix that we try to match with the beginning of the input list
   * @tparam A - the type parameter of the given list and the given prefix
   * @return - true if the given list starts with the given prefix otherwise false
   */
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(ph, pt)) if h == ph => startsWith(t, pt)
    case _ => false
  }

  /**
   * Check whether a given list of A's contain a sublist of A's
   *
   * @param sup - the given list
   * @param sub - the given subsequence (sublist)
   * @tparam A - the parameter type of the lists
   * @return - true if the given list contains the subsequence otherwise false
   */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case(_, Nil) => true
    case (Cons(h, t), Cons(hs, ts)) if (h == hs) => startsWith(sup, sub)
    case (Cons(h, t), Cons(hs, ts)) if (h != hs) => hasSubsequence(t, sub)
    case _ => false
  }
}
