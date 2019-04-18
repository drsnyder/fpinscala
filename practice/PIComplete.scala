import cats._
import cats.syntax.monad._
import cats.implicits._

import scala.concurrent._
import ExecutionContext.Implicits.global

import java.util.Random


trait RNG {
  def nextInt: (Int, RNG)
  def nextDouble: (Double, RNG)
}

case class Point(x: Double, y: Double) {
    val isInCircle = (x * x + y * y) < 1.0
}

case class State[S,+A](run: S => (A, S)) {
  def unit[S, A](a: A): State[S, A] =
    State(state => (a, state))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(state => {
      val (a, newState) = run(state)
      f(a).run(newState)
    })
}

object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (ni, rng2) = rng.nextInt
      if (ni > 0) (ni, rng2)
      else if (ni == Int.MinValue) (0, rng2)
      else (ni + Int.MaxValue, rng2)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (ni, rng2) = nonNegativeInt(rng)
      (ni.toDouble / Int.MaxValue, rng2)
    }


  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def nextDouble: (Double, RNG) = {
      val (n, nextRNG) = nextInt
      double(nextRNG)
    }
  }
}

object PI {
    import RNG._

    def doubleStream(rng: Simple):Stream[Double] = rng.nextDouble match {
        case (d:Double, next:Simple) => d #:: doubleStream(next)
    }

    def estimate(rng: Simple, iter: Int): Double = {
        val doubles = doubleStream(rng).take(iter)
        val inside = (doubles zip doubles.drop(3))
            .map { case (a, b) => Point(a, b) }
            .filter(p => p.isInCircle)
            .size * 1.0
        (inside / iter) * 4.0
    }

    def calculatePi[F[_]](iterations: Int)
                     (random: => F[Double])
                     (implicit F: Monad[F]): F[Double] = {
        case class Iterations(total: Int, inCircle: Int)
        def step(data: Iterations): F[Either[Iterations, Double]] = for {
            x <- random
            y <- random
            isInCircle = (x * x + y * y) < 1.0
            newTotal = data.total + 1
            newInCircle = data.inCircle + (if (isInCircle) 1 else 0)
        } yield {
            if (newTotal >= iterations) Right(newInCircle.toDouble / newTotal.toDouble * 4.0)
            else Left(Iterations(newTotal, newInCircle))
        }
        // iterates until Right value is returned
        F.tailRecM(Iterations(0, 0))(step)
    }
    // calculatePi(10000)(Future { (new Random()).nextDouble() }).onComplete(println)
}