# Free Monads Are Expendable

Free monads are a popular application architecture in functional programming, having nice properties such as:

 - separation of program description from program execution/interpretation,
 - compositionality of interpreters for different instruction sets (a.k.a. algebraas),
 - stack safety (which is a concern only in languages with strict stack-based evaluation).

However, they also come with a price in terms of:
 - _Object allocations:_ We first build up a data structure describing the program.
 - _Pattern matching:_ When interpreting the program description, the interpreter pattern matches on the instructions. For large instruction sets (algebras), this is non-negligible.

This text presents an alternative approach that retains all the nice properties of free monads, but avoids the _pattern matching_ penalty and in some cases also the extra _object allocations_.

The presented approach is how you might design a modular architecture if you had never heard of free monads before.

We will be using the Scala programming language with the [scalaz](https://github.com/scalaz/scalaz) library. Some familiarity with `Free` monads is assumed.

We start with an example using `Free`, and then present its `Free`-less equivalent. 

For completeness, let's list the imports

```tut:silent
import scala.language.higherKinds
import scalaz.{Coproduct, Free, Inject, Monad, |>=|}
import scalaz.syntax.monad._
```

and introduce type aliases that will used below

```tut:silent
type Ref = Long
type Key = String
type Value = String
```


## `Free` example

Let's define some instruction sets.

```tut:silent
sealed trait ValueStoreOp[A]
case class Store(v: Value) extends ValueStoreOp[Ref]
case class Load(ref: Ref) extends ValueStoreOp[Value]
case class Delete(ref: Ref) extends ValueStoreOp[Unit]

// smart constructors for ValueStoreOp instructions
object ValueStoreOp {
  def store[F[_]](v: Value)(implicit i: Inject[ValueStoreOp, F]): Free[F, Ref] =
    Free.liftF(i(Store(v)))
  def load[F[_]](ref: Ref)(implicit i: Inject[ValueStoreOp, F]): Free[F, Value] =
    Free.liftF(i(Load(ref)))
  def delete[F[_]](ref: Ref)(implicit i: Inject[ValueStoreOp, F]): Free[F, Unit] =
    Free.liftF(i(Delete(ref)))
}


sealed trait KeyValueStoreOp[A]
case class Put(k: Key, v: Value) extends KeyValueStoreOp[Unit]
case class Get(k: Key) extends KeyValueStoreOp[Value]
case class Remove(k: Key) extends KeyValueStoreOp[Unit]

// smart constructors for KeyValueStoreOp instructions
object KeyValueStoreOp {
  def put[F[_]](k: Key, v: Value)(implicit j: Inject[KeyValueStoreOp, F]): Free[F, Unit] =
    Free.liftF(j(Put(k, v)))
  def get[F[_]](k: Key)(implicit j: Inject[KeyValueStoreOp, F]): Free[F, Value] =
    Free.liftF(j(Get(k)))
  def remove[F[_]](k: Key)(implicit j: Inject[KeyValueStoreOp, F]): Free[F, Unit] =
    Free.liftF(j(Remove(k)))
}
```

Now, this is how one defines a `Free` program over instructions from both instruction sets above.

```tut:silent
// the combined instruction set
type Op[A] = Coproduct[ValueStoreOp, KeyValueStoreOp, A]

val program: Free[Op, String] = {
  import ValueStoreOp._, KeyValueStoreOp._

  for {
    ref <- store[Op]("Hello")
      a <- load[Op](ref)
      _ <- put[Op]("foo", "world")
      b <- get[Op]("foo")
  } yield s"$a $b"
}
```

To actually execute the free program, one needs to define an interpreter for each instruction set and combine individual interpreters into an interpreter for the combined instruction set `Op`, which is a natural transformation `Op ~> M`, for some target monad `M[_]`. We leave that as an exercise to the reader ;)


## `Free`-less equivalent

The idea is simple: Instead of building a free program over an instruction set, that can then be interpreted into an arbitrary target monad, let's execute the instructions directly, but be _parametric in the target monad_. Instructions no longer need to be reified and later pattern matched on.

The equivalent of an instruction set is a trait with an abstract method for each instruction. The trait itself is parameterized by the target monad:

```tut:silent
trait ValueStore[M[_]] {
  def store(v: Value): M[Ref]
  def load(ref: Ref): M[Value]
  def delete(ref: Ref): M[Unit]
}

trait KeyValueStore[M[_]] {
  def put(k: Key, v: Value): M[Unit]
  def get(k: Key): M[Value]
  def remove(k: Key): M[Unit]
}
```

This is how you then write a program over the above "instruction sets", still parametric in the target monad:

```tut:silent
class Program[M[_]: Monad](
  vs: ValueStore[M],
  kvs: KeyValueStore[M]
) {
  import vs._, kvs._

  def run: M[String] = for {
    ref <- store("Hello")
      a <- load(ref)
      _ <- put("foo", "world")
      b <- get("foo")
  } yield s"$a $b"
}
```

And this is it!

But let's also look at how we run such a program.


### Running the `Free`-less program

An implementation of the "instruction set" trait plays the role of the interpreter. This implementation specifies the target monad.

Now, we could write an interpreter into something like

```scala
StateT[Option, (Map[Ref, Value], Map[Key, Value]), ?]
```

but we will do something else. We will write an interpreter into `Free[Op, ?]`! Why would I do something like that, you ask, if the whole point of this post is to avoid `Free`? The only reason I chose `Free[Op, ?]` as the target monad for this demonstration is that at the same it proves that the `Free`-less approach _doesn't lose any generality_ over `Free`.

In order to support composition with other effects, our implementation will work with any monad that is "greater" than the target monad. A monad `G[_]` is said to be _greater_ than (or equal to) to monad `F[_]`, written `G |>=| F`, if all the effects of `F[_]` are expressible in `G[_]`. `|>=|` is a typeclass in `scalaz`, also called `MonadPartialOrder`.

In a sense, `|>=|` takes the function of `Inject` in Free programs.

```tut:silent
class FreeValueStore[M[_]](implicit i: M |>=| Free[ValueStoreOp, ?]) extends ValueStore[M] {
  def store(v: Value): M[Ref] =
    i(Free.liftF(Store(v)))
  def load(ref: Ref): M[Value] =
    i(Free.liftF(Load(ref)))
  def delete(ref: Ref): M[Unit] =
    i(Free.liftF(Delete(ref)))
}

class FreeKeyValueStore[M[_]](implicit i: M |>=| Free[KeyValueStoreOp, ?]) extends KeyValueStore[M] {
  def put(k: Key, v: Value): M[Unit] =
    i(Free.liftF(Put(k, v)))
  def get(k: Key): M[Value] =
    i(Free.liftF(Get(k)))
  def remove(k: Key): M[Unit] =
    i(Free.liftF(Remove(k)))
}
```

Now that we defined the individual "interpreters", it remains to run the program. Before we can do that, we need to tell the compiler that
 - whenever `Inject[F, G]`, then also `Free[G, ?] |>=| Free[F, ?]`:

```tut:silent
implicit def injectionPartialOrder[F[_], G[_]](implicit i: Inject[F, G]): Free[G, ?] |>=| Free[F, ?] =
  new (Free[G, ?] |>=| Free[F, ?]) {
    implicit val MF: Monad[Free[F, ?]] = implicitly[Monad[Free[F, ?]]]
    implicit val MG: Monad[Free[G, ?]] = implicitly[Monad[Free[G, ?]]]

    def promote[A](fa: Free[F, A]): Free[G, A] = fa.mapSuspension(i)
  }
```

Now we can finally run the parametric program, to obtain the free program:

```tut
val prg: Free[Op, String] = new Program[Free[Op, ?]](new FreeValueStore, new FreeKeyValueStore).run
```


## Discussion

We have demonstrated that free monads are not essential to achieve the first two properties listed above:
 - separation of program description from program execution/interpretation,
 - compositionality of interpreters for different instruction sets.

What about _stack-safety_? A free monad can be interpreter in a stack-safe manner if the target monad supports the [`tailRecM`](http://functorial.com/stack-safety-for-free/index.pdf) operation. This is not the case for our Free-less approach, which cannot take advantage of `tailRecM`. If stack-safety is an issue and our target monad doesn't have lazy `flatMap`s (a.k.a. `bind`s) already, we are left with the old-fashioned solution: just stick a trampoline somewhere in the target monad.

If we are forced to resort to trampolining the target monad, we do not save anything in terms of _object allocations_. However, we still avoid the cost of _pattern-matching_ interpreters.
