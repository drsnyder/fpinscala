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



case class Prop(run: (TestCases,RNG) => Result) {

  def &&(p: Prop) = Prop {
    (c, rng) => run(c, rng) match {
      case Passed => run(c, rng)
      case other => other
    }
  }

  def ||(p: Prop) = Prop {
    (tc, rng) => run(tc, rng) match {
      case Falsified(m, s) => tag(m).run(tc, rng)
      case other => other
    }
  }

  def tag(msg: String) = Prop {
    (n,rng) => run(n,rng) match {
      case Falsified(f, s) => Falsified(msg + f, s)
      case other => other
    }
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
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

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

// FIXME: why did this work? You can't instantiate a trait? Why?
// val rng = RNG.Simple(10)
// val g = Gen(State.unit(rng)) g.listOfN(Gen.unit(10))
case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (a, pa) = g1
    val (b, pb) = g2
    Gen.choose(0, 100).flatMap(r => {
        if (r > pa/100) a
        else {
          if (r > pb/100) b
          else this.union(a, b)
        }
    })
  }

  def unsized: SGen[A] = SGen(_ => this)
}

/**
 * import fpinscala.testing._
 * import fpinscala.state._
 * val rng = RNG.Simple(10)
 * Gen.listOfN(10, State.unit(rng))
 */
object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  val boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))
}


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
}
