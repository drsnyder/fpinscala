sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map(bb => f(aa, bb)))

  def map2f[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else mean(xs)
      .flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def loop(a: List[Option[A]], acc: List[A]): Option[List[A]] = a match {
      case Nil => Some(acc)
      case Some(x) :: xs => loop(xs, x :: acc)
      case _ => None
    }

    loop(a, List())
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x).flatMap(xx => traverse(xs)(f) map (xx :: _))
  }

  def traversef[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, z) => map2f(f(x), z)(_ :: _))


  def sequencet[A](a: List[Option[A]]): Option[List[A]] =
    traversef(a)(aa => aa)
}
