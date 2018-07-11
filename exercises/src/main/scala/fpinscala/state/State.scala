package fpinscala.state

//import fpinscala.laziness.Stream
import scala.collection.immutable.List
//import fpinscala.datastructures._

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

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

  // this blows up with large lists
  //def doubles(rng: RNG): Stream[Double] =
  // Stream.unfold(double(rng))({ case(d, r) => Option(d, (d, r))})

  def mean(ds: Stream[Double]): Double =
    ds.foldRight(0.0)(_ + _) / ds.foldRight(0.0)((_, a) => a + 1.0)


  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n <= 0) (acc, rng)
      else {
        val (i, rngn) = rng.nextInt
        go(n - 1,i :: acc, rngn)
      }
    }

    go(count, List(), rng)
  }

  def intsr(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = intsr(count - 1)(r1)
      (x :: xs, r2)
    }

  def doublem: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, rng1) = ra(rng)
      val (vb, rng2) = rb(rng1)
      (f(va, vb), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def _doubles(count: Int): Rand[List[Double]] =
    sequence(List.fill(count)(double))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (fa, rng2) = f(rng)
      g(fa)(rng2)
    }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(v => unit(f(v)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => _map(rb)(b => f(a, b)))

  def _doublem: Rand[Double] =
    _map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def _both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    _map2(ra, rb)((_, _))

  val _randIntDouble: Rand[(Int, Double)] =
    _both(int, double)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {  v =>
        val mod = v % n
        if (v + (n-1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

import State._

// TODO: 6.10
case class State[S,+A](run: S => (A, S)) {

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

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(state => (a, state))
  
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))


  def sget[S]: State[S, S] = State(s => (s, s))
  def sset[S](s: S): State[S, Unit] = State(_ => ((), s))


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- sget
    _ <- sset(f(s))
  } yield ()

  // val cm = Machine(true, 15, 4)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
