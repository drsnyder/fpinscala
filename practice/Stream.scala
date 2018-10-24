import Stream._
sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(x, xs) => x() :: xs().toList
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
}
