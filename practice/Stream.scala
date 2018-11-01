import Stream._
sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(x, xs) => x() :: xs().toList
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  /*
   * This could be optimized to not look at the stream when n == 1
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(x, xs) if (n > 0) => cons(x(), xs().take(n - 1))
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(x, xs) if (n > 0) => xs().drop(n - 1)
    case _ => this 
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
    case _ => empty
  }

  def takeWhilefr(p: A => Boolean): Stream[A] = 
    foldRight(Stream[A]())((x, z) => 
        if (p(x)) cons(x, z)
        else z
      )

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(x, xs) => f(x(), xs().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] =
    foldRight[Option[A]](None:Option[A])((x, _) => Some(x))

  def nthOption(n: Int): Option[A] =
    drop(n).headOption

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, z) => p(x) && z)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((x, z) => cons(f(x), z))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((x, z) =>
        if (f(x)) cons(x, z)
        else z
      )

  def appendi[B>:A](a: => B): Stream[B] =
    foldRight(Stream[B](a))((x, z) => cons(x, z))

  def append[B>:A](as: => Stream[B]): Stream[B] =
    foldRight(as)((x, z) => cons(x, z))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((x, z) => z.append(f(x)))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
      case (_, Cons(bh, bt)) => Some(((None, Some(bh())), (Empty, bt())))
      case (Cons(ah, at), _) => Some(((Some(ah()), None)), (at(), Empty))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile({ case (a, b) => !b.isEmpty }).forAll({
      case (Some(a), Some(b)) => a == b
      case _ => false
    })

  def tails: Stream[Stream[A]] =
    unfold(this)( {
      case Cons(x, xs) => Some((Stream.cons(x(), xs()), xs()))
      case _ => None
    } )

  def tailsd: Stream[Stream[A]] =
    unfold(this)( {
      case Empty => None
      case s => Some((s, s drop 1))
    } )

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // This won't work because the stopping condition is Empty which we
  // can't use here because the last stream should be z
  def scanRight_1[B](z: => B)(f: (A, => B) => B): Stream[B] =
    unfold(this)( {
      case Empty => Some((z, Empty))
      case s => Some((s.foldRight(z)(f), s drop 1))
    } )

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.map(x => x.foldRight(z)((x, z) => f(x, z)))
        

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fib(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, fib(b, a + b))
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case _ => empty
    }

  def fibsu: Stream[Int] =
    unfold(Stream(0, 1))(s => {
      val a = s.nthOption(0).get
      val b = s.nthOption(1).get
      Some(a, Stream.cons(b, Stream.cons(a + b, s)))
    })

  // uc are simplified versions. There was no need to stream-ify the
  // result
  def fibsuc: Stream[Int] =
    unfold((0, 1))( { case (a, b) => Some(a, (b, a+b)) } )

  def onesu: Stream[Int] = unfold(Stream(1))(s => Some(1, Stream.cons(1, s)))

  def constantu[A](a: A): Stream[A] =
    unfold(Stream(a))(s => Some(a, Stream.cons(a, s)))

  def constantuc[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def fromu(n: Int): Stream[Int] =
    unfold(Stream(n))(s => Some(s.headOption.get, Stream.cons(s.headOption.get + 1, s)))

  def fromuc(a: Int): Stream[Int] =
    unfold(a)(a => Some((a, a + 1)))

  def map[A,B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as) { 
      case Cons(x, xs) => Some((f(x()), xs()))
      case Empty => None
    }

  def take[A](as: Stream[A])(n: Int): Stream[A] =
    unfold((n, as)) {
      case (n, Cons(x, xs)) if n == 0 => None
      case (n, Cons(x, xs)) => Some((x(), (n - 1, xs())))
    }

  def takeWhile[A](as: Stream[A])(f: A => Boolean): Stream[A] =
    unfold(as) { 
      case Cons(x, xs) if (f(x())) => Some((x(), xs()))
      case _ => None
    }

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
      case _ => None
    }


}
