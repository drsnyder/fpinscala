import cats._
import cats.data._
import cats.implicits._

object PICharles {
  type RNG[A] = State[Long, A]

  object RNG {
    def next[A](f: Long ⇒ A): RNG[A] =
      for {
        _ ← State.modify[Long](seed ⇒ (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL)
        s ← State.get
      } yield (f(s))

    val nextInt: RNG[Int] = next(ltoi)
    val nextNatural: RNG[Int] = next(ltoi andThen itonat)
    val nextDouble: RNG[Double] = next(ltoi andThen itonat andThen nattodouble)

    def runRng[A](seed: Long)(rng: RNG[A]): A = rng.runA(seed).value

    def unsafeRunRng[A]: RNG[A] ⇒ A = runRng(System.currentTimeMillis)

    def ltoi: Long ⇒ Int = l ⇒ (l >>> 16).toInt

    def itonat: Int ⇒ Int = i ⇒
      if (i > 0) i
      else if (i == Int.MinValue) 0
      else i + Int.MaxValue

    def nattodouble: Int ⇒ Double = _.toDouble / Int.MaxValue
  }


  object PI {
    case class Step(count: Int, inCircle: Int)

    def calculatePi(iterations: Int): RNG[Double] = {
      def step(s: Step): RNG[Either[Step, Double]] =
        for {
          x ← RNG.nextDouble
          y ← RNG.nextDouble
          isInCircle = (x * x + y * y) < 1.0
          newInCircle = s.inCircle + (if (isInCircle) 1 else 0)
        } yield {
          if (s.count >= iterations)
            Right(s.inCircle.toDouble / s.count.toDouble * 4.0)
          else
            Left(Step(s.count + 1, newInCircle))
        }

      Monad[RNG].tailRecM(Step(0, 0))(step(_))
    }

    def unsafeCalculatePi(iterations: Int) =
      RNG.unsafeRunRng(calculatePi(iterations))

    def ddoubles(iterations: Int): RNG[List[(Double, Double)]] = {
        case class ListStep(count: Int, l: List[(Double, Double)])
        def step(s: ListStep): RNG[Either[ListStep, List[(Double, Double)]]] =
            for {
                x <- RNG.nextDouble
                y <- RNG.nextDouble
            } yield {
                if (s.count >= iterations)
                    Right(s.l)
                else
                    Left(ListStep(s.count + 1, (x, y) :: s.l))
            }
        Monad[RNG].tailRecM(ListStep(0, List[(Double, Double)]()))(step(_))
    }

    def unsafeDdoubles(iterations: Int) =
      RNG.unsafeRunRng(ddoubles(iterations))
  }
}