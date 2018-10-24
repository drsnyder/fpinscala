
// 4.6
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    flatMap(aa => b.map(bb => f(aa, bb)))

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

// 4.7
object Either {
  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] = 
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch { case e: Exception => Left(e)  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, z) => map2(f(x), z)(_ :: _))
}

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]
