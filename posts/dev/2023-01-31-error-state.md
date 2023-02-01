---
title: Recovering from Errors in the State monad
---

I recently encountered an issue where we needed to log information about a
failed task to the database but this information is accumulated over the life
of the task. At first I thought that layering in the `State` monad would solve
our issue.

The problem with the `State` monad in this case is that when an error occurs it's
harder to recover the state. The `State` monad should be used with an immutable
state which is "copied" each time it changes, but if an error occurs you can’t
really peek to see what it was at the time of failure.

```scala
import cats.data.StateT
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

/** The state of our task; keeps track of the `sum` as we `add` to it. */
final case class MyState(sum: Int)

/** Handy type alias representing our Task combined with the State monad. */
type StatefulTask[A] = StateT[Task, MyState, A]

// Some helper functions for producing `StatefulTask`s.
def pure[A](a: A): StatefulTask[A] = StateT.pure(a)
def modify(f: MyState => MyState): StatefulTask[Unit] = StateT.modify(f)
def add(n: Int): StatefulTask[Unit] = modify(s => s.copy(sum = s.sum + n))
def get: StatefulTask[MyState] = StateT.get

// An example task in which an exception occurs. We want to log the state
// along with the error, but as you'll see this is impossible by the
// time the exception can be handled.
val statefulTask: StatefulTask[Int] = for {
  a <- pure(1)
  _ <- add(a)
  b <- StateT.liftF(Task.pure(2))
  _ <- add(b)
  c <- StateT.liftF(Task.eval[Int](throw new RuntimeException("boom")))
  _ <- add(c)
  state <- get
} yield state.sum

// Breaking down each step and its type for clear demonstration.
val task: Task[(MyState, Int)] = statefulTask.run(MyState(0))
val attempt: Task[Either[Throwable, (MyState, Int)]] = task.attempt
val result: Either[Throwable, (MyState, Int)] = attempt.runSyncUnsafe()
result.fold(
  throwable => println(s"ERROR: $throwable"),
  { case (state, sum) => println(s"STATE: $state; SUM: $sum") },
)
```

The result of this is -

```
ERROR: java.lang.RuntimeException: boom
```

As you can see, in the `throwable` case, we don't have access to `MyState`. So
if an error occurs, we're stuck out.

One step in the right direction could be to using `EitherT` around `StateT` so
the state can be recovered -

```scala
import cats.data.{EitherT, StateT}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

final case class MyState(sum: Int)

// Defining `Internal` to avoid the need for a type lambda.
type Internal[A] = StateT[Task, MyState, A]
type StatefulTask[A] = EitherT[Internal, Throwable, A]

def pure[A](a: A): StatefulTask[A] = EitherT.pure(a)
def modify(f: MyState => MyState): StatefulTask[Unit] = EitherT.liftF(StateT.modify(f))
def add(n: Int): StatefulTask[Unit] = modify(s => s.copy(sum = s.sum + n))
def get: StatefulTask[MyState] = EitherT.liftF(StateT.get)

/** Lifts a vanilla `Task` through both `StateT` and `EitherT`. */
def liftTask[A](task: Task[A]): StatefulTask[A] = EitherT.liftF(StateT.liftF(task))

// Same example as before.
val statefulTask: StatefulTask[Int] = for {
  a <- pure(1)
  _ <- add(a)
  b <- liftTask(Task.pure(2))
  _ <- add(b)
  c <- liftTask(Task.eval[Int](throw new RuntimeException("boom")))
  _ <- add(c)
  state <- get
} yield state.sum

val stateT: StateT[Task, MyState, Either[Throwable, Int]] = statefulTask.value
val task: Task[(MyState, Either[Throwable, Int])] = stateT.run(MyState(0))
val (state, result) = task.runSyncUnsafe()
println(s"STATE: $state")
result.fold(
  throwable => println(s"ERROR: $throwable"),
  sum => println(s"SUM: $sum"),
)
```

However, the problem here is that the `RuntimeException` gets thrown from inside
the `Task` and does not propagate to the `EitherT`, so we still lose the state!

```
java.lang.RuntimeException: boom
  at $anonfun$statefulTask$5(<console>:6)
  ... 16 elided
```

To get around this, we **MUST** ensure that any `Task` that gets executed uses
`.attempt` so its exceptions are captured by the `EitherT` -

```scala
def liftTask[A](task: Task[A]): StatefulTask[A] = EitherT(StateT.liftF(task.attempt))
```

Then we get the following -

```
STATE: MyState(3)
ERROR: java.lang.RuntimeException: boom
```

However, this is extremely easy to mess up. If you have one stray task where you
haven't totally shaken the error out, you'll lose your state. This is precisely
what happened in the prior example. Any `Task` that is to be eventually turned
into a `StatefulTask` at some point needs to wrap it in our special `liftTask`
function. It requires extreme programmer diligence and thus is something that
will inevitably go wrong.

For this reason many in the Haskell world prefer using `ReaderT` over an
`IORef` which contains a mutable reference. That's essentially what we're doing
in this next example, except in Scala we don't need `IORef` and can just rely on
mutable fields.

```scala
import cats.data.ReaderT
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

final case class MyState(var sum: Int)

type StatefulTask[A] = ReaderT[Task, MyState, A]

def pure[A](a: A): StatefulTask[A] = ReaderT.pure(a)
def modify(f: MyState => Unit): StatefulTask[Unit] = ReaderT(s => Task.pure(f(s)))
def add(n: Int): StatefulTask[Unit] = modify(s => s.sum += n)
def get: StatefulTask[MyState] = ReaderT.ask

def liftTask[A](task: Task[A]): StatefulTask[A] = ReaderT.liftF(task)

val statefulTask: StatefulTask[Int] = for {
  a <- pure(1)
  _ <- add(a)
  b <- liftTask(Task.pure(2))
  _ <- add(b)
  c <- liftTask(
    Task.eval[Int](throw new RuntimeException("boom"))
  )
  _ <- add(c)
  state <- get
} yield state.sum

val state = MyState(0)
val task: Task[Int] = statefulTask.run(state)
val attempt: Task[Either[Throwable, Int]] = task.attempt
val result: Either[Throwable, Int] = attempt.runSyncUnsafe()
println(s"STATE: $state")
result.fold(
  throwable => println(s"ERROR: $throwable"),
  sum => println(s"SUM: $sum"),
)
```

And the output -

```
STATE: MyState(3)
ERROR: java.lang.RuntimeException: boom
```

No need for any special error handling required by those constructing tasks,
stateful or otherwise; any `Task` can be properly handled this way at the very end when we actually run
our `StatefulTask`.
