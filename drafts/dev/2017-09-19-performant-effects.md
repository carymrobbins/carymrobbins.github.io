# Programming with Effects

You may have heard of the elusive `IO` monad from Haskell. You may have even used
it or one of its analogues in other languages. Currently, there are many competing
ideas for how effects should be modeled in Scala. Among them are
[cats-effect](https://github.com/typelevel/cats-effect),
[scalaz-effect](https://github.com/scalaz/scalaz),
[Monix](https://github.com/monix/monix),
and probably many more. There is also
[active work being done](http://degoes.net/articles/only-one-io)
to bring something
much closer to Haskell's IO to Scala.

But many of you may not understand what effects are useful for in the first
place. I'm going to demonstrate a real-world use case how effects can be
used as a tool to model your program correctly, provide useful abstractions,
and avoid bugs.

## IntelliJ Read Context

Many of you may already know that I currently work on
[HaskForce](https://github.com/carymrobbins/intellij-haskforce),
an IntelliJ plugin for Haskell.

When developing plugins for IntelliJ, often you will need to access data
that can only be obtained in a _read context_. This can be done by either
being in the UI thread or explicitly calling `Application#runReadAction()`.
If you fail to follow the rules, you will end up with runtime exceptions.

For instance, let's say we have a `PsiElement` which refers to an element
in a parse tree (when parsing Haskell source code for analysis).
The `PsiElement#getChildren` method must be executed in a read context.
If we don't get the threading rules right, our users will get errors
at runtime. Let's see if we can leverage the compiler to help prevent this.

## Approach #1: Implicit Parameter

This is many people's first instinct: to pass a token as an implicit parameter.
The Scala standard library actually does this with `Future` and `ExecutionContext`.
This is actually a **bad idea**, but for now let's entertain the idea.

We'll create a wrapper class that provides safe access to the element's children -

```scala
final case class SPsiElement[+A <: PsiElement](toPsiElement: A) {
  def getChildren(implicit token: ReadToken): Array[SPsiElement] = {
    toPsiElement.getChildren.map(SPsiElement(_))
  }
}

```
