// import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S,B] =
    this.flatMap(a => State.unit(f(a)))

  def map2[B,C](rb: State[S,B])(f: (A, B) => C): State[S,C] =
    this.flatMap(a => rb.map(b => f(a, b)))

  def flatMap[B](g: A => State[S,B]): State[S,B] = State(s => {
    val (fa, sp) = this.run(s)
    g(fa).run(sp)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  type Rand[A] = State[RNG, A]

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def simulateMachineA(inputs: List[Input]): State[Machine, (Int, Int)] = State( machine => {
    val last = inputs.foldLeft(machine)({ 
      case (machine, input) => update(input)(machine)
    })
    ((last.coins, last.candies), last)
  })
		

}

// simulateMachine(List(Coin, Turn)).run(Machine(false, 0, 0))
// def add1(a: Int): Int = a + 1
// State.modify(add1).run(1)
// State.simulateMachineA(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))
// res87: ((Int, Int), Machine) = ((14,1),Machine(true,1,14))
//
