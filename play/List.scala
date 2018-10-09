
sealed trait List[+A]
final case object Nil extends List[Nothing]
final case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, Cons(x, xs))
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def dropWhilet[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhilet(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f)) 
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRViaL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((z, x) => f(x, z))

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_+_)

  def suml(ints: List[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  def lengthl[A](as: List[A]): Int =
    foldLeft(as, 0)((z, _) => z + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((z, x) => Cons(x, z))

  def append[A](as: List[A], x: A): List[A] =
    foldRight(as, List[A](x))((x, z) => Cons(x, z))

  def appendl[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)((x, z) => Cons(x, z))

  def prepend[A](as: List[A], x: A): List[A] =
    reverse(foldLeft(as, List[A](x))((z, x) => Cons(x, z)))

  def concat[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)(Cons(_,_))

  def add1(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((x, z) => Cons(x + 1, z))

  def dToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((x, z) => Cons(x.toString, z))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x, z) => Cons(f(x), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, z) => 
        if (f(x)) Cons(x, z)
        else z
    )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((x, z) => appendl(f(x), z))

  def zip[A, B](as: List[A], bs: List[B]): List[(A,B)] = {
    @annotation.tailrec
    def go(as: List[A], bs: List[B], acc: List[(A,B)]): List[(A,B)] = (as, bs) match {
      case (Nil, Nil) => acc
      case (_, Nil) => acc
      case (Nil, _) => acc
      case (Cons(ah, at), Cons(bh, bt)) => go(at, bt, Cons((ah, bh), acc))
    }
    reverse(go(as, bs, List[(A,B)]()))
  }

  def add2l(a: List[Int], b: List[Int]): List[Int] = 
    map(zip(a, b)) { case (x, y) => x + y }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    map(zip(as, bs)) { case (x, y) => f(x, y) }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(lh, lt), Cons(ph, pt)) if (lh == ph) => startsWith(lt, pt)
      case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case _ if (startsWith(sup, sub)) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
