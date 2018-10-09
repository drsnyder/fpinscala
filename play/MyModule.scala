object MyModule {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, im1: Int, im2: Int): Int =
      if (n == 0) im1
      else go(n - 1, im2, im2 + im1)

    go(n, 0, 1)
   }

  def isSorted[Thing](as: Array[Thing], ordered: (Thing,Thing) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n)))
        loop(n + 1)
      else false

    loop(1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def partial1[AType,BType,CType](a: AType, f: (AType,BType) => CType): BType => CType =
    b => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
