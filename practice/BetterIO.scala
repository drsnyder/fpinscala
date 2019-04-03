import fpinscala.monads._

sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f) // we do not interpret the `flatMap` here, just return it as a value
  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] { // Notice that none of these operations DO anything
  def unit[A](a: => A): IO[A] = Return(a)
  def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
  def suspend[A](a: => IO[A]) =
      Suspend(() => ()).flatMap { _ => a }
}

@annotation.tailrec def run[A](io: IO[A]): A = io match {
  case Return(a) => a
  case Suspend(r) => r()
  case FlatMap(x, f) => x match {
    case Return(a) => run(f(a))
    case Suspend(r) => run(f(r()))
    case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
  }
}

def printLine(s: String): IO[Unit] =
  Suspend(() => Return(println(s)))

object BetterIO {
  import IO._
  val p = IO.forever(printLine("Still going..."))

  val f: Int => IO[Int] = (x: Int) => Return(x)
  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f){
      (a: Function1[Int, IO[Int]],
        b: Function1[Int, IO[Int]]) => {
        (x: Int) => IO.suspend(a(x).flatMap(b))
      }
    }
}
