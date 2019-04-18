import fpinscala.state.RNG
import fpinscala.state.RNG._
// import cats.data.State
import scalaz._, Scalaz._

object PI2 {

    def dbl(rng: RNG): (RNG, Double) = {
      val (ni, rng2) = nonNegativeInt(rng)
      (rng2, ni.toDouble / Int.MaxValue)
    }

    val nextDouble: State[RNG, Double] = State(rng => dbl(rng))

    val dpair: State[RNG, (Double, Double)] = for {
        d1 <- nextDouble
        d2 <- nextDouble
    } yield (d1, d2)

    val t = (0 to 10).toList.traverseS(dpair)(RNG.Simple(10))
}