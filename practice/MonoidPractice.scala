import fpinscala.testing._
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    val zero = (a: A) => a
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A,B)] {
    def op(a: (A,B), b: (A,B)): (A, B) = (a, b) match { case ((aa, ab), (ba, bb)) => (A.op(aa, ba), B.op(ab, bb)) }
    def zero = (A.zero, B.zero)
  }


  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(x: A => B, y: A => B): A => B = (a: A) => B.op(x(a), y(a))
    val zero = (a: A) => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val bagger = mapMergeMonoid[A, Int](intAddition)
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(bagger)
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
      } yield (x, y, z)) { 
        t =>
          val (a, b, c) = t
          m.op(a, m.zero) == (m.op(m.zero, a)) &&
          m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
    }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((x, z) => m.op(z, f(x)))
    //as.map(f).fold(m.zero)(m.op)
    //as.foldLeft(m.zero)((z, x) => m.op(z, f(x)))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def stringToCount(s: String): Int =
    if (s.isEmpty) 0
    else 1

  sealed trait WC {
    def count: Int = this match {
      case Stub(a) => stringToCount(a)
      case Part(l, c, r) => stringToCount(l) + c + stringToCount(r)
    }
  }

  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def wordsFromString(s: String): List[String] =
    ("\\w+".r.findAllIn(s)).toList

  def stringToWc(s: String): WC = {
    val words = wordsFromString(s)
    val leftGap = s.startsWith(" ")
    val rightGap = s.endsWith(" ")
    (leftGap, words.size, rightGap) match {
      case (_, 0, _) => Stub("")
      case (true, 1, false) => Part("", 0, words.head)
      case (true, 1, true) => Part("", 1, "")
      case (false, 1, true) => Part(words.head, 1, "")
      case (false, 1, false) => Stub(words.head)

      case (true, n, false) => Part("", n-1, words.last)
      case (true, n, true) => Part("", n, "")
      case (false, n, true) => Part(words.head, n-1, "")
      case (false, n, false) => Part(words.head, n-2, words.last)
    }
  }

  def combine(x: WC, y: WC): WC = (x, y) match {
    case (Stub(a), Stub(b)) => Stub(a + b)
    case (Stub(a), Part(l, c, r)) => Part(a + l, c, r)
    case (Part(l, c, r), Stub(b)) => Part(l, c, r + b)
    case (Part(ll, lc, lr), Part(rl, rc, rr)) =>
     Part(ll, lc + wordsFromString(lr + rl).size + rc, rr)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(x: WC, y: WC): WC = combine(x, y)
    val zero = Stub("")
  }

  def wordCount(s: String): Int = {
    if (s.length <= 100) {
      return stringToWc(s).count
    } else {
      val (l, r) = s.splitAt(s.length/2)
      return stringToWc(l).count + stringToWc(r).count
    }
  }

  def wc(s:String): WC = {
    if (s.isEmpty) Stub(s)
    else {
      val (h, r) = s.splitAt(10)
      wcMonoid.op(stringToWc(h), wc(r))
    }
  }

  // answer
  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMap(s.toIndexedSeq.toList, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

}


trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_::_)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as match {
    case x :: xs => f(x, foldRight(xs)(z)(f))
    case Nil => z
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as match {
    case x :: xs => foldLeft(xs)(f(z, x))(f)
    case Nil => z
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).fold(mb.zero)(mb.op)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).fold(mb.zero)(mb.op)
}

// object StreamFoldable extends Foldable[Stream] {
//   override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
//     ???
//   override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
//     ???
// }

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
  } 

  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
  }

  def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }
}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case Some(a) => f(a, z)
    case None => z
  }

  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case Some(a) => f(z, a)
    case None => z
  }

  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Some(a) => mb.op(f(a), mb.zero)
    case None => mb.zero
  }
}

object MonoidTest {

  def run() = {
    Prop.run(Monoid.monoidLaws(Monoid.stringMonoid, Gen.string.apply(100)))

    val ints = Gen.choose(-100, 100)
    Prop.run(Monoid.monoidLaws(Monoid.intMultiplication, ints))

    // Both produce the same result
    List("a", "b", "c").foldRight(Monoid.stringMonoid.zero)(
      Monoid.stringMonoid.op)
    List("a", "b", "c").foldLeft(Monoid.stringMonoid.zero)(
      Monoid.stringMonoid.op)

    println(
      ListFoldable.foldRight(List("a", "b", "c"))("")(_ + _)
    )
    println(
      ListFoldable.foldLeft(List("a", "b", "c"))(Monoid.stringMonoid.zero)(Monoid.stringMonoid.op)
    )

    println(
      IndexedSeqFoldable.foldRight(List(1,2,3).toIndexedSeq)(0)(_+_)
    )

    assert(
      TreeFoldable.foldRight(
        Branch(Leaf("a"),
        Branch(Leaf("b"),Leaf("c")))
     )("")(_ + _).equals("abc")
    )

    assert(
      TreeFoldable.foldLeft(
        Branch(Leaf("a"),
        Branch(Leaf("b"),Leaf("c")))
     )("")(_ + _).equals("abc")
    )

    assert(
      TreeFoldable.foldMap(
        Branch(Leaf("a"),
        Branch(Leaf("b"),Leaf("c")))
     )(x => x)(Monoid.stringMonoid).equals("abc")
    )

    assert(
      TreeFoldable.toList(
        Branch(Leaf("a"),
        Branch(Leaf("b"),Leaf("c")))
     ).equals(List("a", "b", "c"))
    )

    println(
      Monoid.bag(Vector("a", "rose", "is", "a", "rose"))
    )

    val stubs = Gen.stringN(100).map(s => Monoid.Stub(s))
    Prop.run(Monoid.monoidLaws(Monoid.wcMonoid, stubs))

    val m = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
    val p = ListFoldable.foldMap(List(1,2,3,4))(a => (1, a))(m)
    assert(
      p.equals((4, 10))
    )

  }

}