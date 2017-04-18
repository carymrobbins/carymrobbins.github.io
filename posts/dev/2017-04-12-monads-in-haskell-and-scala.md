---
title: Monads in Haskell and Scala
---

Since the rise in popularity of Scala, more and more people are
coming into contact with Monads (whether they realize it or not).
For instance, any time you use the `for` construct, you are using
a Monad.

```scala
def optAdd(ox: Option[Int], oy: Option[Int]): Option[Int] = {
  for {
    x <- ox
    y <- oy
  } yield x + y
}
```

For the unfamiliar, `Option[A]` has two constructors, `Some(x: A)` or `None`.

As you may have been able to tell, this function will return
`Some` if and only if both of its arguments are `Some`. If either is `None`,
the result will be `None`.

```scala
scala> optAdd(Some(1), Some(2))
res0: Option[Int] = Some(3)

scala> optAdd(Some(1), None)
res1: Option[Int] = None

scala> optAdd(None, Some(2))
res2: Option[Int] = None

scala> optAdd(None, None)
res3: Option[Int] = None
```

So how does the compiler know what to do? Does Scala treat `Option`
in some sort of special way?

To answer these questions, let's desugar the magical `for` syntax.

```scala
def optAddWithFor(ox: Option[Int], oy: Option[Int]): Option[Int] = {
  for {
    x <- ox
    y <- oy
  } yield x + y
}

def optAddWithoutFor(ox: Option[Int], oy: Option[Int]): Option[Int] = {
  ox.flatMap { x =>
    oy.map { y =>
      x + y
    }
  }
}
```
