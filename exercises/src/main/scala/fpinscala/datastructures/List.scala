package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }


  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case xs => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0 || l == Nil) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f)
    else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => y + 1)


  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def suml(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productl(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthl[A](l: List[A]): Int =
    foldLeft(l, 0)((x, y) => x + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(h, t) => append(reverse(t), List(h))
  }

  def reversel[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((z, x) => Cons(x, z))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, y) => f(y, x))

  def appendf[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](as: List[List[A]]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => foldRight(h, concat(t))((x, y) => append(List[A](x), y))
  }

  def addone(ints: List[Int]): List[Int] =
    foldRight(ints, List[Int]())((x, y) => Cons(x + 1, y))

  def dToString(doubles: List[Double]): List[String] =
    foldRight(doubles, List[String]())((x, y) => Cons(x.toString, y))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((x, y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, y) =>
      if (f(x)) Cons(x, y)
      else y)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(reverse(as), List[B]())((z, x) => append(f(x), z))
  // concat(map(l)(f))

  def filterfm[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) =>
      if (f(x)) List(x)
      else Nil
    )

  def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh+rh, addPairwise(lt, rt))
  }

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh,rh), zipWith(lt, rt)(f))
  }



  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(lh, lt), Nil) => true
    case (Nil, Nil) => true
    case (Nil, Cons(sh, st)) => false
    case (Cons(lh, lt), Cons(sh, st)) =>
      if (lh == sh)
        hasSubsequence(lt, st)
      else
        hasSubsequence(lt, sub)
  }

}
