package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/**
 * Pick up on section 8.4
 */

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// trait Prop {
//   def check: Either[(FailedCase, SuccessCount), SuccessCount]
  // val prop =
  //    forAll(intList)(ns => ns.reverse.reverse == ns) &&
  //    forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  // First version
  // Creates a new Prop and defines what check should mean
  //def &&(p: Prop): Prop = new Prop {
    //def check = Prop.this.check && p.check
  //}
// }


case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(p: Prop) = Prop {
    (m, c, rng) => run(m, c, rng) match {
      case Passed | Proved => p.run(m, c, rng)
      case other => other
    }
  }

  def ||(p: Prop) = Prop {
    (mx, tc, rng) => run(mx, tc, rng) match {
      case Falsified(m, s) => tag(m).run(mx, tc, rng)
      case other => other
    }
  }

  def tag(msg: String) = Prop {
    (m,n,rng) => run(m,n,rng) match {
      case Falsified(f, s) => Falsified(msg + f, s)
      case other => other
    }
  }
}
/*
import fpinscala.testing._
import fpinscala.state._
import fpinscala.parallelism._
val rng = RNG.Simple(10)
val smallInt = Gen.choose(-10,10)

// fails because of empty lists
val maxProp = Prop.forAll(Gen.listOf(smallInt)) { (ns:List[Int]) =>
  val max = ns.max
  !ns.exists(_ > max)
}
maxProp.run(10, 10, RNG.Simple(System.currentTimeMillis))

// with the new helper function on object Prop
Prop.run(maxProp, 10, 10)

val maxPropNe = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}
Prop.run(maxPropNe, 4, 4)

val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
  if (ns.isEmpty) true
  else {
    val sorted = ns.sorted
    val h = sorted.head
    val t = sorted.last
    h <= t
  }
}
Prop.run(sortedProp, 10, 10)

Prop.randomStream(smallInt)(RNG.Simple(System.currentTimeMillis)).take(5).zip(Stream.from(0)).take(5).toList
Stream.from(0).take(5).map(i => Gen.listOf(smallInt)(i).sample.run(RNG.Simple(System.currentTimeMillis))).toList

def lt3(i: Int): Boolean = i < 3
def twt(ns: List[Int]): Boolean =
  ns.takeWhile(lt3).dropWhile(lt3).isEmpty &&
  ns.takeWhile(lt3) ++ ns.dropWhile(lt3) == l

  Works manually but not in the test
val takeWhileProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
  println(ns)
  ns.takeWhile(lt3).dropWhile(lt3).isEmpty &&
  ns.takeWhile(lt3) ++ ns.dropWhile(lt3) == l
}
Prop.run(takeWhileProp, 10, 10)

*/


object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)  }
    }.find(_.isFalsified).getOrElse(Passed)
  }


  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println("+ OK, passed tests.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  //val p2e = checkPar { equal (
    //Par.map(Par.unit(1))(_ + 1),
    //Par.unit(2)
  //)
  //}

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val p3 = check {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    ) (ES) get
  }

  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](S: Gen[ExecutorService])(g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }
  }

  def checkPar(S: Gen[ExecutorService])(p: Par[Boolean]): Prop =
    forAllPar(S)(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }
}

// FIXME: why did this work? You can't instantiate a trait? Why?
/*
import fpinscala.testing._
import fpinscala.state._
import fpinscala.parallelism._
import java.util.concurrent.Executors

val rng = RNG.Simple(10)
val g = Gen(State.unit(rng))
g.listOfN(Gen.unit(10))

g.union(Gen.unit(1), Gen.unit(2)).sample.run(rng)
g.weighted((Gen.unit(1), 0.9), (Gen.unit(2), 0.2)).sample.run(RNG.Simple(2))

def even(i: Int): Boolean = (i % 2) == 0
val c = Gen.choose(0, 10)
val d = Gen.choose(11, 20)
Prop.forAll(c)(x => x < 9).run(10, rng)
Prop.forAll(c)(even).run(10, rng)

val a = Prop.forAll(c)(even) && Prop.forAll(d)(even)
a.run(10, rng)

val t = Prop.forAll(c)(i => i >= 0) || Prop.forAll(d)(even)
t.run(10, rng)


val S = Gen.weighted(Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75, Gen.unit(Executors.newCachedThreadPool) -> .25)
val pint = Gen.choose(0,10) map (Par.unit(_))
Prop.run(Prop.forAllPar(S)(pint)(n => Prop.equal(Par.map(n)(y => y), n)))

val fint = Gen.choose(0,10) map (i => Par.fork(Par.unit(i)))
Prop.run(Prop.forAllPar(S)(fint)(n => Prop.equal(Par.map(n)(y => y), n)))
*/
case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  // def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
  def map2[B,C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap(a => gb.map(b => f(a, b)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf1: SGen[List[A]] =
    Gen.listOf1(this)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def unitOption[A](a: Gen[A]): Gen[Option[A]] =
    a.map(x => Option(x))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))(
      (a, b) => (a, b)
    )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  val boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  // Does this belong here because the input is g: Gen[A]?
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (a, pa) = g1
    val (b, pb) = g2
    val g1Threshold = pa.abs / (pa.abs + pb.abs)
    Gen.choose(0, 100).flatMap(r => {
        if (r < g1Threshold) a
        else b
    })
  }


  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)
}
/*
 import fpinscala.testing._
 import fpinscala.state._
 val rng = RNG.Simple(10)
 val c = Gen.choose(0, 100)
 c.sample.run(rng)
 Gen.listOfN(10, c).sample.run(rng)

 val chars = ('a' to 'z').toList
 val ci = Gen.choose(0, chars.length)
 Gen.listOfN(10, ci.map(i => chars(i))).sample.run(rng)

 // 8.5+
 // generating random strings
 // doesn't work as expected
 Gen.listOfN(10, ci.map2(Gen.unit(""))((i, acc) => acc + chars(i))).sample.run(rng)

 // Produces State(random-string, RNG)
 Gen.listOfN(10, ci.map(i => chars(i)))
   .map(l => l.foldRight("")((a,z) => z+a))
   .sample.run(rng)

 val c2 = Gen.choose2(0, 100)
 c2.sample.run(rng)


 */


//trait Gen[A] {
  //def map[A,B](f: A => B): Gen[B] = ???
  //def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize(_).flatMap(a => f(a)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
}
/*

 c.unsized.map(i => i + 1)(2).sample.run(rng)
 Gen.choose(0, 10).unsized.listOf(Gen.unit(5))(3).sample.run(rng)
 // Produces:
 // (List[Int], fpinscala.state.RNG) = (List(5, 5, 5),Simple(10))
 // maybe this is a more proper void?
 Gen(State.unit(None)).unsized.listOf(Gen.unit(5))(3).sample.run(rng)
 */
