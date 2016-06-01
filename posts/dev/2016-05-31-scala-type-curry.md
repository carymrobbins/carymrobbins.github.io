---
title: Currying Type Parameters in Scala
---

Scala doesn't provide facilities to supply only some of the type parameters
required by a method.  If you wish to supply _any_ of them, you must supply _all_ of them.

For instance, let's say we wished to create a method to safely down-cast a value
from one type to a possible subtype, but enforce at compile-time that the target
type is indeed a subtype of the value's class.  Here's a possible implementation -


```scala
import scala.reflect.ClassTag

def down[B >: A, A : ClassTag](b: B): Option[A] = {
  implicitly[ClassTag[A]].unapply(b)
}
```

While this works, we must specify both the target subtype _and_ the input type -

```
scala> down[Any, Int](1)
Option[Int] = Some(1)

scala> down[Any, Int]("foo")
Option[Int] = None

scala> down[CharSequence, String]("foo": CharSequence)
Option[String] = Some(foo)
```

Ideally, we'd like the input type inferred by the compiler, but unfortunately, Scala
syntax doesn't permit this.

One [clever way](http://stackoverflow.com/a/29202291/1313611) of getting around this
might be to use an anonymous class to create a structural type -

```scala
def down[A] = new {
  def apply[B >: A](b: B)(implicit ct: ClassTag[A]): Option[A] = {
    ct.unapply(b)
  }
}
```

Note that when using our new method, we'll get the following warning -

```
scala> down[Int](1: Any)
<console>:13: warning: reflective access of structural type member method apply should be enabled
by making the implicit value scala.language.reflectiveCalls visible.
This can be achieved by adding the import clause 'import scala.language.reflectiveCalls'
or by setting the compiler option -language:reflectiveCalls.
See the Scala docs for value scala.language.reflectiveCalls for a discussion
why the feature should be explicitly enabled.
       down[Int](1: Any)
           ^
Option[Int] = Some(1)
```

In order to continue using the method and not receive warnings from its usage,
we'd need to avoid the anonymous class or enable `reflectiveCalls`.  I'll
demonstrate how we can avoid the anonymous class and `reflectiveCalls`,
but first let's ensure our logic works as intended.

```scala
scala> val x: Any = 1

scala> down[Int](x)
Option[Int] = Some(1)

scala> down[String](x)
Option[String] = None

scala> val x: Int = 1

scala> down[String](x)
Option[String] = None
```

While this works, it doesn't seem to enforce our compile-time check that the
input type is a proper supertype of our target subtype.  The problem here has to 
do with _type lubbing_, which basically means that the compiler will infer type parameters
as the nearest common supertype.  This becomes more apparent if we output the input
type's class -

```scala
def down[A] = new {
  def apply[B >: A](b: B)(implicit ct: ClassTag[A], ct2: ClassTag[B]): Option[A] = {
    println("** B == " + ct2.runtimeClass)
    ct.unapply(b)
  }
}
```

```
scala> down[String](1: Int)
** B == class java.lang.Object
Option[String] = None
```

To avoid type lubbing, we can enforce that evidence exists proving that A is a subtype of B.
This works since the evidence is checked _after_ the `B` type parameter is inferred.

```scala
def down[A] = new {
  def apply[B](b: B)(implicit ct: ClassTag[A], ev: A <:< B): Option[A] = {
    ct.unapply(b)
  }
}
```

```scala
scala> down[String]("foo": CharSequence)
Option[String] = Some(foo)

scala> down[String](1: Int)
<console>:10: error: Cannot prove that String <:< Int.
              down[String](1: Int)
```

Now the compiler rejects the cases it can prove will never succeed.  While you
could stop here, I have a few issues with this implementation -

* We have warnings to address (as I prefer to use `-Xfatal-warnings`)
* The method return type is inferred, while I prefer public methods to have explicit
    return types to make things simpler for us humans.
* The inferred return type is a structural type, of course, and that's messy to annotate.

```scala
scala> down[Int]
AnyRef{def apply[A](a: A)(implicit ct: scala.reflect.ClassTag[Int],implicit ev: <:<[Int,A]): Option[Int]} = $anon$1@6b85300e
```

Ok, so let's knock out three birds with one stone and wrap it in a utility object -

```scala
object CastUtil {

  def down[A]: _Down[A] = _down.asInstanceOf[_Down[A]]

  final class _Down[A] private[CastUtil] {
    def apply[B](b: B)(implicit ct: ClassTag[A], ev: A <:< B): Option[A] = {
      ct.unapply(b)
    }
  }
  private lazy val _down = new _Down[Nothing]
}
```

We define an explicit class to handle our downcasting.  This avoids the anonymous
class, avoids `reflectiveCalls`, and gives us clean and explicit return types.  We
define the `lazy val _down` to give us a singleton instance of the `_Down` class to avoid
creating a new instance every time we call the method.

```
scala> CastUtil.down[Int]
CastUtil._Down[Int] = CastUtil$_Down@2e6a8155

scala> CastUtil.down[Int]
CastUtil._Down[Int] = CastUtil$_Down@2e6a8155

scala> // ^^ same instance both times

scala> CastUtil.down[String]("foo": CharSequence)
Option[CharSequence] = Some(foo)

scala> CastUtil.down[String](1: Any)
res9: Option[String] = None

scala> CastUtil.down[String](null: CharSequence)
Option[CharSequence] = None

scala> CastUtil.down[String](1: Int)
<console>:10: error: Cannot prove that String <:< Int.
              CastUtil.down[String](1: Int)
                                   ^
```
