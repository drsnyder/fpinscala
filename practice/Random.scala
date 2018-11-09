// import Random._

trait RNG {
	def nextInt: (Int, RNG)
}



object Random {

  // transition function signature
  // A randomly generated A
  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (fa, rng1) = f(rng)
    g(fa)(rng1)
  }

  def map_1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2_1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })

  def nonNegativeLessThan_1(n: Int): Rand[Int] = { rng => 
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan_1(n)(rng)
  }

  def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List[A](), rng))((x, z) => {
      val (a, rng1) = x(z._2)
      (a :: z._1, rng1)
    })
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    
  def intss(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(rngn => rngn.nextInt))
  

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = 
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    (Math.abs(i), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng1)
  }

  def doublem: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)
  

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (d1, rng2) = double(rng)
    ((i1, d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1, d1), rng1) = intDouble(rng)
    ((d1, i1), rng1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def iter(c: Int, acc: List[Int], rngi: RNG): (List[Int], RNG) =
      if (c == 0) (acc, rngi)
      else {
        val (i, rng1) = rngi.nextInt
        iter(c - 1, i :: acc, rng1)
      }
    
    iter(count, List(), rng)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}
