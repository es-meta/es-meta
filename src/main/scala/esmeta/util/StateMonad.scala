package esmeta.util

class StateMonad[S] {
  // result type
  trait Result[+T] extends (S => (T, S)) {
    // map function
    def map[U](f: T => U): Result[U] = s => {
      val (v, s0) = this(s)
      (f(v), s0)
    }

    // flat map function
    def flatMap[U](f: T => Result[U]): Result[U] = s => {
      val (v, s0) = this(s)
      f(v)(s0)
    }

    // get the value of the result
    def eval(s: S): T = this(s)._1
  }

  // convert function to result type
  def id[T](f: S => (T, S)): Result[T] = s => f(s)

  // update type
  type Updater = S => S

  // get the current state
  def get: Result[S] = s => (s, s)

  // get a property of the current state
  def get[A](f: S => A): Result[A] = s => (f(s), s)

  // modify the current state
  implicit def modify(f: Updater): Result[Unit] = s => ((), f(s))

  // conversion to updater
  implicit def toUpdater(m: Result[_]): Updater = s => m(s)._2

  // list of updaters to list of results
  implicit def listModify(list: List[Updater]): List[Result[Unit]] =
    list.map(modify)

  // put a new state as the current state
  def put(s: S): Result[Unit] = _ => ((), s)

  // create a state monad with a value
  implicit def pure[T](v: T): Result[T] = s => (v, s)

  // join a list of state monads to a state monad of lists
  def join[T](iter: Iterable[Result[T]]): Result[List[T]] =
    iter.foldRight(pure(List[T]())) {
      case (lm, rm) =>
        for {
          l <- lm
          r <- rm
        } yield l :: r
    }
  def join[T](iter: Iterable[Updater]): Updater =
    iter.foldRight[Updater](st => st) {
      case (f, g) => st => g(f(st))
    }

  def withState[T](f: S ?=> T): Result[T] =
    for {
      given S <- get
      v = f
    } yield v

  def concretize[T](f: S ?=> Result[T]): Result[T] =
    for {
      given S <- get
      v <- f
    } yield v
}
