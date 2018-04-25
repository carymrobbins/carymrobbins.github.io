---
title: Merging Scala Case Classes with Shapeless and Monoid
---

The other day at work I came across a structure similar the following. The data,
types, and logic have all been greatly simplified to protect you from irrelevant
noise.

```scala
final case class Info(
  name: Option[String] = None,
  city: Option[String] = None,
  friendIds: Vector[Int] = Vector.empty
)
```

Essentially, all of our fields are either `Option` or `Vector`. Later we have some code
that's building our `Info` data using something like the following. Again, note that
this is all very simplified to keep things digestible.

```scala
// Query for profile info, setting the name and city fields if they exist.
def getProfileInfo(id: Int): Future[Option[Info]] = {
  db.run(profileQuery(id)).map {
    case None    => None
    case Some(x) => Some(Info(name = x.name, city = x.city))
  }
}

// Query for friend info, setting the friendIds field.
def getFriendInfo(id: Int): Future[Option[Info]] = {
  db.run(friendQuery(id)).map {
    case None    => None
    case Some(x) => Some(Info(friendIds = x.friendIds))
  }
}

// Fetch all info sources, joining the results.
def getAllInfo(id: Int): Future[Either[String, Info]] =
  for {
    optProfileInfo <- getProfileInfo(id)
    optFriendInfo  <- getFriendInfo(id)
  } yield (optProfileInfo, optFriendInfo) match {
    case (Some(profileInfo), Some(friendInfo)) =>
      Right(profileInfo.copy(friendIds = friendInfo.friendIds))
    case (Some(profileInfo), None) =>
      Right(profileInfo)
    case (None, Some(friendInfo)) =>
      Right(friendInfo)
    case (None, None) =>
      Left("Failed to fetch info")
  }
```

That last `getAllInfo` method stuck out to me. It seemed that all we were really trying to do
there was combine `Info` values. Sound familiar? Data types which are combinable form a
`Monoid` (fancy math term from Category Theory). For this example, we'll be using the cats library.
Be sure to check out the [cats documentation for Monoid](https://typelevel.org/cats/typeclasses/monoid.html).

TL;DR - `Monoid` is a type class which contains two methods -

```scala
trait Monoid[A] {
  def combine(x: A, y: A): A
  def empty: A
}
```

Astute readers may notice that I've left out `Monoid`'s superclass `Semigroup`, which is actually
where its `combine` method comes from. `Monoid` builds on `Semigroup` by introducing the `empty`
method, sometimes also referred to as the "identity". As such, `Monoid` is often defined as a
`Semigroup` with _identity_. Not really a prerequisite for understanding this article, but worth
pointing out for completeness.

Let's define a `Monoid` instance for `Info` -

```scala
object Info {
  implicit val monoid: Monoid[Info] = new Monoid[Info] {
    override def empty: Info = Info()
    override def combine(x: Info, y: Info): Info = {
      val Info(name1, city1, friendIds1) = x
      val Info(name2, city2, friendIds2) = y
      Info(name1.orElse(name2), city1.orElse(city2), friendIds1 ++ friendIds2)
    }
  }
}
```

Then our `getAllInfo` logic can become -

```scala
import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._

def getAllInfo(id: Int): Future[Either[String, Info]] =
  for {
    optProfileInfo <- getProfileInfo(id)
    optFriendInfo  <- getFriendInfo(id)
  } yield List(
    optProfileInfo,
    optFriendInfo
  ).combineAll.toRight("Failed to fetch info")
```

Much simpler right? We're using the `Option` instance for `Monoid` combined with
the `List` instance for `Foldable` to get the `.combineAll` method which builds
up an `Option[Info]` from our `List[Option[Info]]`. We then convert that
resulting `Option[Info]` to an `Either[String, Info]` using `.toRight`.

Now, our Monoid definition is just a little tedious in this case, but imagine we have more fields on our case class. Writing this
by hand can get a little unwieldy and error-prone (particularly since our case class has default fields).
Can we abstract this further?

Let's look at that `Monoid` definition again, specifically where we're combining the fields -

```scala
Info(name1.orElse(name2), city1.orElse(city2), friendIds1 ++ friendIds2)
```

So for `Option` values we need to use `.orElse`, and for `Vector` values we need to
use `++`. Is there some abstraction that does this for us? Why yes, yes there is!

Enter [MonoidK](https://typelevel.org/cats/typeclasses/monoidk.html). It's very similar to `Monoid` except
it operates on a _higher kind_. It has a superclass: `SemigroupK`, but we'll simplify the definition
for now.

```scala
trait MonoidK[F[_]] {
  def combineK[A](x: F[A], y: F[A]): F[A]
  def empty[A]: F[A]
}
```

So we could abstract away our hand-written combines with -

```scala
import cats.MonoidK.ops._
import cats.instances.vector._
import cats.instances.option._
...
Info(name1.combineK(name2), city1.combineK(city2), friendIds1.combineK(friendIds2))
```

But that doesn't really feel much better. It still requires us to write each field by hand and,
strictly by programmer diligence, to not forget a field. Let's try to abstract away the
field boilerplate with [shapeless](https://github.com/milessabin/shapeless)' `Generic` type class.

```scala
scala> import shapeless._

scala> val g = Generic[Info]
g: shapeless.Generic[Info]{type Repr = Option[String] :: Option[String] :: scala.collection.immutable.Vector[Int] :: shapeless.HNil} = anon$macro$8$1@381fe083

scala> g.to(Info())
res1: g.Repr = None :: None :: Vector() :: HNil

scala> g.to(Info(Some("foo"), None, Vector(1, 2)))
res2: g.Repr = Some(foo) :: None :: Vector(1, 2) :: HNil

scala> g.from(res3)
res3: Info = Info(Some(foo),None,Vector(1, 2))
```

`Generic` allows us to convert our case class into an `HList` and back again.
The intermediate `HList` representation provides us with ways to perform generic
operations on the structure before we re-assemble it.

Let's introduce our own type class called `GMerge` which will provide generic
merging functionality for types which have a `Generic` instance (which includes
all case classes) where all of the fields support `MonoidK`.

```scala
trait GMerge[A] {
  def merge(x: A, y: A): A
}

object GMerge {

  // Provide a helper method for summoning a GMerge instance.
  def apply[A: GMerge]: GMerge[A] = implicitly

  // Auto-derive an instance of GMerge.
  implicit def default[A]: GMerge[A] = new GMerge[A] {
    override def merge(x: A, y: A): A = ???
  }
}
```

Goal now is to implement the `default` method which derives our `GMerge` instance.
Let's start by just attempting to think about the logical flow.

1. Obtain the generic representation (`HList`) of both `x` and `y`
2. Zip the `HList` values together, producing an `HList` of `Tuple2` (`(B, B)`)
3. Run a merging function over the zipped `HList`, merging the elements of each
   `Tuple2` together with `MonoidK`, returning a new `HList` matching the original
   type structure.
4. Convert the "merged" `HList`

In the case of `Info`, the types would look something like -

```scala
val x = Info(Some("foo"), None, Vector(1, 2))
val y = Info(None, Some("bar"), Vector.empty)
val g = Generic[Info]

val hlistX: Option[String] :: Option[String] :: Vector[Int] :: HNil = g.to(x)
// Some(foo) :: None :: Vector(1, 2) :: HNil

val hlistY: Option[String] :: Option[String] :: Vector[Int] :: HNil = g.to(y)
// None :: Some(bar) :: Vector() :: HNil

val zipped: (Option[String], Option[String]) ::
            (Option[String], Option[String]) ::
            (Vector[Int],    Vector[Int]) ::
            HNil = hlistX.zip(hlistY)
// (Some(foo),None) :: (None,Some(bar)) :: (Vector(1, 2),Vector()) :: HNil
```

You can actually run all of the code above in an Scala console for some defined
`x` and `y` values. The next part, however, won't work, but demonstrates the
principle of what we need to do.

```scala
def f = ??? // Function for merging fields.
val hlistR: Option[String] :: Option[String] :: Vector[Int] :: HNil = zipped.map(f)
val result: Info = g.from(hlistR)
```

So somehow we need to build a function which can be mapped over an `HList` of
`Tuple2`, merging the fields accordingly with `MonoidK`. Let's take a whack at it.

```scala
def mergeTuple[F[_], V](t: (F[V], F[V]))(implicit F: MonoidK[F]): F[V] = t match {
  case (x, y) => F.combineK(x, y)
}
```

Let's try it out -

```scala
scala> mergeTuple((Option.empty[String], Option("bar")))
res10: Option[String] = Some(bar)

scala> mergeTuple((Option("foo"), Option("bar")))
res11: Option[String] = Some(foo)

scala> mergeTuple((Vector(1), Vector(2)))
res12: scala.collection.immutable.Vector[Int] = Vector(1, 2)
```

Alright! Let's supply it to `.map` and be done with this.

```scala
scala> zipped.map(mergeTuple)
<console>:13: error: polymorphic expression cannot be instantiated to expected type;
 found   : [F[_], V](t: (F[V], F[V]))(implicit F: cats.MonoidK[F])F[V]
 required: shapeless.Poly
       zipped.map(mergeTuple)
                  ^
```

Huh? We gave it a perfectly fine function to work with, but it wants us to give it
a...`Poly`? Why?

The problem is that simple (non-generic) data structures like `List` are homogeneously
typed: they only really contain elements of a single type.

```scala
scala> List((None, Option("bar")), (Option("baz"), None))
res14: List[(Option[String], Option[String])] = List((None,Some(bar)), (Some(baz),None))

scala> res14.map(mergeTuple[Option, String])
res15: List[Option[String]] = List(Some(bar), Some(baz))
```

In the above case, our `List` elements are all typed `(Option[String], Option[String])`
and there is no way to have different types at different element positions.
Contrast this with `HList` -

```scala
scala> hlistX
res16: Option[String] :: Option[String] :: Vector[Int] :: shapeless.HNil = Some(foo) :: None :: Vector(1, 2) :: HNil
```

Here each element has its own type. Scala doesn't have a built-in way to deal with
generic polymorphism (not to be confused with _generics_ which is actually parametric
polymorphism). This is where shapeless' `Poly` comes in.

`Poly` provides _polymorphic function_ support. There are arity variants such as `Poly1`,
`Poly2`, etc. Let's reimplement our `mergeTuple` function in terms of `Poly1` -

```scala
object polyMerge extends Poly1 {
  implicit def cases[F[_], V](implicit F: MonoidK[F]): Case.Aux[(F[V], F[V]), F[V]] =
    at[(F[V], F[V])] { case (x, y) => F.combineK(x, y) }
}
```

To get a deeper understanding of how `Poly` works, check out the
[Shapeless 2.0 overview documentation](https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#polymorphic-function-values).

Let's take our new `polyMerge` function for a spin -

```scala
scala> polyMerge((Vector(1), Vector(2)))
res17: scala.collection.immutable.Vector[Int] = Vector(1, 2)

scala> polyMerge((Option.empty[String], Option("bar")))
res18: Option[String] = Some(bar)

scala> polyMerge((Option("foo"), Option("bar")))
res19: Option[String] = Some(foo)
```

It works! Now we can add in our `polyMerge` function into our `Info` example.
Picking up from where we left off -

```scala
val hlistR: Option[String] :: Option[String] :: Vector[Int] :: HNil = zipped.map(polyMerge)
// Some(foo) :: Some(bar) :: Vector(1, 2) :: HNil

val res: Info = g.from(hlistR)
// Info(Some(foo),Some(bar),Vector(1, 2))
```

Nice! One simplification we can make is to use `.zipWith` instead of `.zip().map()`.
This also means that we need to use `Poly2` instead of `Poly1`, which actually turns out
to be simpler -

```scala
object polyMerge extends Poly2 {
  implicit def cases[F[_], V](implicit F: MonoidK[F]): Case.Aux[F[V], F[V], F[V]] =
    at[F[V], F[V]](F.combineK)
}
```

```scala
val hlistR = hlistX.zipWith(hlistY)(polyMerge)
// Some(foo) :: Some(bar) :: Vector(1, 2) :: HNil
```

Alright, let's try to put it all together now into our `default` method
and get to auto-deriving already!

When working with Shapeless, compiler errors can be a little cryptic,
so I'll walk through each step, demonstrating how to resolve them as we
encounter each one.

```scala
implicit def default[A]: GMerge[A] = new GMerge[A] {
  override def merge(x: A, y: A): A = {
    val g = Generic[A]
    val hlistX = g.to(x)
    val hlistY = g.to(y)
    val hlistR = hlistX.zipWith(hlistY)(polyMerge)
    g.from(hlistR)
  }
}
```

```scala
Error:(45, 20) could not find implicit value for parameter gen: shapeless.Generic[A]
    val g = Generic[A]
```

Following the compiler's orders, let's add a `Generic` constraint.

```scala
implicit def default[A](
  implicit
  g: Generic[A]
): GMerge[A] = new GMerge[A] {
  override def merge(x: A, y: A): A = {
    val hlistX = g.to(x)
    val hlistY = g.to(y)
    val hlistR = hlistX.zipWith(hlistY)(polyMerge)
    g.from(hlistR)
  }
}
```

```scala
Error:(50, 27) value zipWith is not a member of g.Repr
      val hlistR = hlistX.zipWith(hlistY)(polyMerge)
```

Problem now is that the compiler doesn't know that our generic `Repr` is
an `HList`. Let's tell it.

```scala
implicit def default[A, L <: HList](
  implicit
  g: Generic.Aux[A, L]
): GMerge[A] = new GMerge[A] {
  override def merge(x: A, y: A): A = {
    val hlistX = g.to(x)
    val hlistY = g.to(y)
    val hlistR = hlistX.zipWith(hlistY)(polyMerge)
    g.from(hlistR)
  }
}
```

```scala
Error:(50, 42) could not find implicit value for parameter
  zipWith: shapeless.ops.hlist.ZipWith[g.Repr,g.Repr,polyMerge.type]
      val hlistR = hlistX.zipWith(hlistY)(polyMerge)
```

This part might be a little more confusing, so let's look at (a simplified version
of) the `ZipWIth` type class.

```scala
trait ZipWith[L <: HList, R <: HList, P <: Poly2] { type Out <: HList }
object ZipWith {
  type Aux[L <: HList, R <: HList, P <: Poly2, Out0 <: HList] =
    ZipWith[L, R, P] { type Out = Out0 }
}
```

The type arguments correspond as follows -
* `L` - The left `HList` being zipped
* `R` - The right `HList` being zipped
* `P` - The `Poly` function being applied
* `Out` - The return type of `zipWith`, which is the `HList` type returned
  from applying our `Poly` function to each of its elements.

In our case the left and right element types of each `HList` should be the same since we're
just merging `A`s together, and the return type should also be the same. For the `Poly`
type, we can name function directly with `polyMerge.type`. This gives us a constraint
that looks like -

```scala
z: ZipWith.Aux[L, L, polyMerge.type, L]
```

Putting it all together gives us -

```scala
implicit def default[A, L <: HList](
  implicit
  g: Generic.Aux[A, L],
  z: ZipWith.Aux[L, L, polyMerge.type, L]
): GMerge[A] = new GMerge[A] {
  override def merge(x: A, y: A): A = {
    val hlistX = g.to(x)
    val hlistY = g.to(y)
    val hlistR = hlistX.zipWith(hlistY)(polyMerge)
    g.from(hlistR)
  }
}
```

And it compiles! Now let's try to use it to derive a `combine` implementation
for `Monoid[Info]` -

```scala
object Info {
  implicit val monoid: Monoid[Info] = new Monoid[Info] {
    override def empty: Info = Info()
    override def combine(x: Info, y: Info): Info = GMerge[Info].merge(x, y)
  }
}
```

```scala
Error:(24, 58) could not find implicit value for evidence parameter of type example.GMerge[example.Info]
    override def combine(x: Info, y: Info): Info = GMerge[Info].merge(x, y)
```

This error is pretty cryptic and will happen if you are missing one of the necessary
implicits to allow `polyMerge` to work for your case class. In this case, we need
to bring in the `MonoidK` instances for `Option` and `Vector`, so let's do that -

```scala
import cats.instances.vector._
import cats.instances.option._
```

And it compiles!

For completeness, here's the full implementation. It's a good practice
to keep the `Info` case class and `GMerge` trait in their own respective
files, but for simplicity (yet again) just including it in a single, working
snippet.

```scala
import cats.{Monoid, MonoidK}
import cats.instances.vector._
import cats.instances.option._
import shapeless._
import shapeless.ops.hlist.ZipWith

final case class Info(
  name: Option[String] = None,
  city: Option[String] = None,
  friendIds: Vector[Int] = Vector.empty
)

object Info {
  implicit val monoid: Monoid[Info] = new Monoid[Info] {
    override def empty: Info = Info()
    override def combine(x: Info, y: Info): Info = GMerge[Info].merge(x, y)
  }
}

trait GMerge[A] {
  def merge(x: A, y: A): A
}

object GMerge {

  def apply[A: GMerge]: GMerge[A] = implicitly

  implicit def default[A, L <: HList](
    implicit
    g: Generic.Aux[A, L],
    z: ZipWith.Aux[L, L, polyMerge.type, L]
  ): GMerge[A] = new GMerge[A] {
    override def merge(x: A, y: A): A = g.from(g.to(x).zipWith(g.to(y))(polyMerge))
  }

  object polyMerge extends Poly2 {
    implicit def cases[F[_], V](implicit F: MonoidK[F]): Case.Aux[F[V], F[V], F[V]] =
      at[F[V], F[V]](F.combineK)
  }
}
```

A good next step would be to write a test for your case class to ensure that your
`Monoid` instance obeys the laws.

```scala
import cats.kernel.laws.discipline.MonoidTests
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class InfoSpec extends FunSuite with Discipline {
  implicit val arbInfo: Arbitrary[Info] = ???
  checkAll("Monoid[Info]", MonoidTests[Info].monoid)
}
```
