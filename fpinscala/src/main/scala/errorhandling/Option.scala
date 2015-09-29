package errorhandling {

  import scala.{Either => _, None => _, Option => _, Some => _}

  /**
   * Created by oltyant on 2/9/15.
   * Implement all the methods in the <i>Option</i> trait. As you implement each function, try to think about what it means
   * and in what situations you'd use it. We'll explore when to use each of these functions next. Here are a few hints
   * for solving this exercise:
   * <ul>
   * <li>It is fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse
   *   without resorting to pattern matching</li>
   * <li>For <i>map</i> and <i>flatMap</i>, the type signature should be enough to determine the implementation.</li>
   * <li><i>getOrElse</i> returns the result inside the <i>Some</i> case of the <i>Option</i>, or if the <i>Option</i> is
   * <i>None</i> returns the given default value</li>
   * <li><i>orElse</i> returns the first <i>Option</i> if it is defined; otherwise, it returns the second <i>Option</i>.
   */
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

    protected def filter_def(f: A => Boolean): Option[A] = flatMap((a: A) => if (f(a)) this else None)

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    protected def orElse_def[B >: A](ob: => Option[B]): Option[B] = map((a: A) => this) getOrElse ob
  }

  case object None extends Option[Nothing]

  case class Some[+A](get: A) extends Option[A]

  object Option {
    def failingFn(i: Int): Int = {
      lazy val y: Int = throw new Exception("fail!")
      try {
        val x = 42 + 5
        x + y
      } catch {
        case e: Exception => 43
      }
    }

    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    /**
     * Implement the vatriance function in terms of flatMap. If the mean of sequence is m, the variance is the mean
     * of <i>sum(math.pow(x - m, 2.)/n)</i> for each element of x in the sequence
     * @param xs - the input sequence of Doubles
     * @return - the variance
     */
    def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    def variance0(xs: Seq[Double]): Option[Double] = {
      val m = mean(xs) getOrElse 0.0
      Some(xs.map(x => math.pow(x - m, 2.0) / xs.length)).flatMap((xs: Seq[Double]) => Some(xs.sum))
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    val absO: Option[Double] => Option[Double] = lift(math.abs)

    /**
     * Write a generic function <i>map2</i> that combines two <i>Option</i> values using a binary function.
     * If either <iyOption</i> value is <i>None</i>, then return value is too.
     * @param a - the first parameter of the combined Option
     * @param b - the second parameter of the combined Option
     * @param f - a function that combines an input type A with an input type B and returns type C
     * @tparam A - first input type parameter
     * @tparam B - second input type parameter
     * @tparam C - the type parameter of the result
     * @return - the combined Option of type C
     */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))

    def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = a.flatMap(x => b.flatMap(y => c.map(z => f(x,y,z))))

    def map2for[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
      x <- a
      y <- b
    } yield f(x, y)

    /**
     * Write a function <i>sequence</i> that combines a lis of <i>Options</i> into one Option containing a lis of all
     * the <i>Some</i> values in the original list. If the original list contains <i>None</i> even once,
     * the result of the function should be <i>None</i>; otherwise the result should be <i>Some</i> with a list of all the values.
     *
     * @param a - the input list of type A variables
     * @tparam A - the input's type parameter
     * @return - the input list wrapped in an Option
     */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h match {
        case None => None
        case s @ Some(v) => sequence(t).map(x => v :: x)
      }
    }

    def sequence0[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hv => sequence(t).map(hv :: _))
    }

    def sequenceFoldRight[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h match {
        case None => None
        case s @ Some(v) => t.foldRight[Option[List[A]]](Some(List(v)))((a: Option[A], acc: Option[List[A]]) => map2(a, acc)((a: A, b: List[A]) => a :: b))
      }
    }

    def sequenceFoldRight0[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((a: Option[A], acc: Option[List[A]]) => map2(a, acc)((a: A, b: List[A]) => a :: b))

    /**
     * Implement traverse function in a similar way than the sequence implemented as it is straightforward
     * how can we implement it with sequence and map. Nonetheless in this way we traverse the list twice
     * which is not the most efficient way
     * @param a - input list of A typed elements
     * @param f - a function that maps a type A to Option B
     * @tparam A - the parameter type of the input list
     * @tparam B - the result parameter type
     * @return - the input list mapped to B typed elements and wrapped into an Option
     */
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))

    def traverseFold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

    def traverseMap[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(v => traverseMap(t)(f).map(xs => v :: xs))
    }
  }


}
