import fpinscala.monads._

sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
        new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] =
        new IO[B] { def run = f(self.run).run }
}

object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    // def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
    // sealed class IORef[A](var value: A) {
    //     def set(a: A): IO[A] = IO { value = a; a }
    //     def get: IO[A] = IO { value }
    //     def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    // }

}

def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
def ReadLine: IO[String] = IO { readLine }


object SimpleIO {
    import IO._

    def readWrite: IO[Unit] = for {
        _ <- PrintLine("Enter a string: ")
        s <- ReadLine
        _ <- PrintLine(s)
    } yield ()

    val lines: IO[List[String]] = replicateM(10)(ReadLine)
    val p = IO.forever(PrintLine("Still going..."))
}