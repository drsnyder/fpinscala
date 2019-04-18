import fpinscala.state._
import fpinscala.iomonad._
import fpinscala.monads._

case class Point(x: Double, y: Double) {
    val isInCircle = (x * x + y * y) < 1.0
}

object PI {
    import State._
    import RNG._
    // import Monad._
    // // import IO3._
    // import Monad._
    // import IO2a._

    // type SStream[A] = State[RNG, A]
    // type FSStream[A] = Free[SStream, A]
    // // def double(s: RNG): FSStream[(Double, RNG)] =
    // //     freeMonad.unit(RNG.double(s))

    // def get[S]: State[S, S] = State(s => (s, s))

    // def set[S](s: S): State[S, S] = State(_ => (s, s))

    // def modify[S](f: S => S): State[S, S] = for {
    //     s <- get
    //     ns <- set(f(s))
    // } yield ns

    // // This is not stack safe because of doubles. Can we implement
    // // doubles using the Free monad so it's stack safe?

    // def estimate(rng: RNG, iterations: Int): Double = {
    //     val doubles = RNG.doubles(iterations)(rng)._1
    //     val points = (doubles zip doubles.drop(3))
    //         .map { case (x, y) => Point(x, y) }
    //     val inside = points
    //         .filter(p => p.isInCircle)
    //     println(inside.size)
    //     (inside.size / iterations.toDouble) * 4.0
    // }

    // val doubles = stateMonad.replicateM(100)(modify[(Double, RNG)](r => RNG.double(r._2)))
    //     .run(RNG.double(RNG.Simple(10)))
    //     ._2
    // // val fDoubles = freeMonad.replicateM(100)(double(RNG.Simple(10)))
    //     //.run(RNG.double(RNG.Simple(10)))

    // def iodoubles(n: Int, rng: RNG, l: List[Double]): IO[(List[Double], RNG)] = {
    //     val (d, rng1) = RNG.double(rng)
    //     IO.unit((d +: l, rng1)).flatMap(t => {
    //         if (n > 0)
    //             iodoubles(n - 1, t._2, t._1)
    //         else
    //             IO.unit(t)
    //     })
    // }
    // val ds = IO2a.run(PI.iodoubles(10000000, RNG.Simple(10), List()))

    // // Goal- stack safe State monad
    // // What if we created a specialized state monad?
    // case class TState[S,A](run: S => IO[(A, S)]) {
    //     def map[B](f: A => B): TState[S, B] =
    //         flatMap(a => TState.unit(f(a)))
    //     def flatMap[B](f: A => TState[S, B]): TState[S, B] =
    //         TState(state => {
    //             run(state).flatMap( { case (a, newState) => f(a).run(newState) })
    //         })
    // }

    // object TState {
    //     def unit[S, A](a: A): TState[S, A] =
    //         TState(state => IO.unit((a, state)))
    // }

    // Try this with cats or
    // https://github.com/typelevel/cats/blob/774fb51d1365e1adef7fa71f09b1410941264f60/tests/src/test/scala/cats/tests/RegressionSuite.scala
    // IS an implicit required?
    //val dpair: State[Simple, (Double, Double)] =
    val nextDouble: State[RNG, Double] = State(rng => RNG.double(rng))
    val dpair: State[RNG, (Double, Double)] =
        for {
            d1 <- nextDouble
            d2 <- nextDouble
        } yield (d1, d2)
    // PI.dpair.run(RNG.Simple(10))

    def dpairf(rng: RNG): (Double, Double) =
        dpair.run(rng)._1

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



}
