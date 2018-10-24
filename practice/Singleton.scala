object Singleton {
  def abs(n: Int): Int =
    if (n < 0)
      -n
    else n

  def main(args: Array[String]): Unit = 
    println("hello %d %d".format(-10, abs(-10)))
}
