import fpinscala.monads._
import fpinscala.testing._
import fpinscala.state._
import scala.util.Random

/*
To run a monte carlo simulation we need as inputs
 - drift: computed from historical data = average daily return - (variance ÷ 2)
 - stddev from the historical data
 - today's price

 random value = standard deviation * NORMSINV(RAND())
 next day's price = today's price * e ^ (drift + random value)

 Gen could be a good starting place to model from.


 */

// see https://github.com/scalaz/scalaz/blob/7e64f43872fc944b54a63e4262acaea2248f50c8/example/src/main/scala/scalaz/example/StateTUsage.scala
object MonadPractice {
    import Monad._
    import Reader._

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, S] = State(_ => (s, s))

    type IntPair = (Int, Int)
    def modify[S](f: S => S): State[S, S] = for {
        s <- get
        ns <- set(f(s))
    } yield ns

    val F = stateMonad[Int]
    def zipWithIndex[A](as: List[A]): List[(Int,A)] =
        as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
            xs <- acc
            n  <- get
            _  <- set(n + 1)
        } yield (n, a) :: xs).run(0)._1.reverse

    def run() = {
        println(
             modify[Int](i => i + 1).run(5)
        )

        println(
            stateMonad.replicateM(3, modify[Int](i => i + 1)).run(4)
        )

        println(
            stateMonad.replicateM(8, modify[IntPair](
                { case (a, b) => (b, a + b) }
            )).run((0, 1))
        )

        println(
            stateMonad.replicateM(8, modify[IntPair](
                { case (a, b) => (b, a + b) }
            )).run((0, 1))
            ._1.map(_._2)
        )

        println(
            stateMonad.sequence(List.fill(10)(modify[Int](i => i + 1))).run(5)
        )

        println(
            optionMonad.replicateM(4, Option(1))
        )

        optionMonad.filterM(List(Some(1), Some(2)))(c => Option(true))
        println(
            listMonad.filterM(List('a', 'b'))(c => List(false, false))
        )

        val lhs = optionMonad.compose((x: Int) => Some(x + 1), (y: Int) => optionMonad.unit(y))
        val rhs = optionMonad.compose((x: Int) => optionMonad.unit(x), (y: Int) => Some(y + 1))

        val range = Gen.choose(-100, 100)
        Prop.run(Prop.forAll(range) { ns => lhs(ns) == rhs(ns) })

        for { a <- Id("Hello, "); b <- Id("monad!") } yield a + b

        println(
            readerMonad[String].replicateM(3, Reader((s: String) => s.toUpperCase)).run("five")
        )
    }
}