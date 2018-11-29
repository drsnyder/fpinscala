import java.util.concurrent.{ExecutorService,Future,Callable,TimeUnit}


/*
 * Note that Par is not an actual data type so we can't do
 * x.map2(y)(f). According to the chapter notes there is a
 * way to do this with implicits.
 */
object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit[List[A]](List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    as.foldRight(unit[List[A]](List[A]()))((x, acc) => map(acc)(a => 
        if (f(x))
          x :: a
        else a
    ))
  }

  def parFoldRight[A,B](as: List[A], start: B)(f: (A, B) => B): Par[B] = fork {
    as.foldRight(unit[B](start))((a, zz) => map(zz)(z => f(a, z)))
  }

  def countWords(s: String): Int = s.split(" ").length

  def totalWords(pgs: List[String]): Par[Int] =
    parFoldRight(pgs, 0)((s, count) => count + countWords(s))

  // Isn't this simpler with foldRight above?
  // def splitApply[A,B](a: IndexedSeq[A], z: B)(f: (B, B) => B): Par[B] = fork {
  //   if (a.length < 1)
  //     Par.unit(z)
  //   else {
  //     val (l,r) = a.splitAt(a.length/2)
  //     Par.map2(Par.fork(splitApply(l,z)(f)), Par.fork(splitApply(r,z)(f)))(f)
  //   }
  // }

  def splitApplyInt(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] = fork {
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(splitApplyInt(l)(f)), Par.fork(splitApplyInt(r)(f)))(f)
    }
  }

  def max(as: IndexedSeq[Int]): Par[Int] = splitApplyInt(as)(_ max _)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

}

// import java.util.concurrent.ForkJoinPool
// def add1(a:Int): Int = a + 1
// Par.asyncF(add1)(1)
// val f = Par.run(new ForkJoinPool())(Par.asyncF(add1)(1))
// f.get()
//
// Par.run(new ForkJoinPool())(Par.sortPar(Par.unit(List(1, 3, 2, 5, 4, 0))))
// val f = Par.run(new ForkJoinPool())(Par.parMap(List(1, 2, 3, 4))((i:Int) => i + 1))
// f.get()
// val f = Par.run(new ForkJoinPool())(Par.parFilter(List(1, 2, 3, 4))((i:Int) => i > 1))
// f.get()
// val f = Par.run(fjp)(Par.parFoldRight(List(1, 2, 3, 4), 0)(_ + _))
// val f = Par.run(fjp)(Par.splitApplyInt(IndexedSeq(1, 2, 3, 4))(_ max _))
// 
// val pgs = List("some words", "a", "other words")
// val f = Par.run(fjp)(Par.totalWords(pgs))
//
// Par.equal(fjp)(Par.map(Par.unit(1))(_ + 1), Par.unit(2))
//
// Forcing deadlock and breaking fork(x) == x
// val a = lazyUnit(42 + 1)
// val S = Executors.newFixedThreadPool(1)
// println(Par.equal(S)(a, fork(a)))
//
