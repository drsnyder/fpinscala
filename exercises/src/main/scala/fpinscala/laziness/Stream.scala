package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def toList: List[A] =
    this.foldRight(List[A]())((a, b) => a :: b)


  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // wrong
  def takeb(n: Int): Stream[A] = this match {
    case Cons(h, t) => if (n == 0) this else cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => this
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>  cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhilefr(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)


  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionfr: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  // def map[A,B](l: List[A])(f: A => B): List[B] =
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B>:A](l: Stream[B]): Stream[B] =
    foldRight(l)((h, t) => cons(h, t))

  def flatMap[B>:A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this)({
      case Cons(h, t) => Some(cons(h(), t()), t())
      case _ => None
    })

  //def mapViaUnfold[B](f: A => B): Stream[B] =
  def mapu[B](f: A => B): Stream[B] =
    unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def takeu(n: Int): Stream[A] =
    unfold(this)({
      case Cons(h, t) if n > 1 => Some(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Some(h(), empty)
      case _ => None
    })

  def takeWhileu(p: A => Boolean): Stream[A] =
    unfold(this)({
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2))({
      case (Cons(lh, lt), Cons(rh, rt)) => Option(((Option(lh()), Option(rh())), (lt(), rt())))
      case (Cons(lh, lt), _) => Option(((Option(lh()), None), (lt(), Empty)))
      case (_, Cons(rh, rt)) => Option(((None, Option(rh())), (Empty, rt())))
      case _ => None
    })

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B >: A](start: B)(f: (B, B) => B): Stream[B] =
    tails.map(e => e.foldRight(start)((a, b) => f(a, b))).append(Stream(start))
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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def repeatedly[A](f: () => A): Stream[A] =
    Stream.cons(f(), repeatedly(f))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  def fibs(): Stream[Int] = {
    def go(ip: Int, i: Int): Stream[Int] =
      Stream.cons(ip, go(i, ip + i))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case _ => empty
  }

  def onesu: Stream[Int] = unfold((1, 1))(s => Option(1, s))

  def fibsu(): Stream[Int] =
    unfold((0, 1))((s: (Int, Int)) =>
      s match { case(ip, i) => Option(ip, (i, ip + i)) })

  def constantu[A](a: A): Stream[A] =
    unfold((a, a))(s => Option(a, s))

  def fromu(n: Int): Stream[Int] =
    unfold((n, n + 1))({ case(p, n) => Option(p, (n, n + 1)) })

  def zipWith[A,B,C](l: Stream[A], r: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((l, r))({
      case (Cons(lh, lt), Cons(rh, rt)) => Option(f(lh(), rh()), (lt(), rt()))
      case (Empty, _) => None
      case (_, Empty) => None
    })

  // this doesn't work if prefix is longer
  def startsWithz[A](l: Stream[A], prefix: Stream[A]): Boolean =
    !zipWith(l, prefix)((l, r) => l == r).exists(_ == false)

  def hasSubsequence[A](s: Stream[A], sub: Stream[A]): Boolean =
    s.tails.map(t => t.startsWith(sub)).exists(p => p)


  // def hasSubsequence[A](sup: Stream[A], sub: Stream[A]): Boolean =
  //   unfold((sup, sub))({
  //     case (a, b) => if (!zipWith(a, b)((x, y) => x == y).exists(p => p == false)) Some((true), empty)
  //   })

}
