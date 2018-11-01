trait RNG {
	def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object Random {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    (Math.abs(i), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng1)
  }

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
}
