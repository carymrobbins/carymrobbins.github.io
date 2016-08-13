---
title: Intro to Scala Macros
---

Scala macros can be daunting at first, but once you get the hang of them
they become an invaluable tool for removing boilerplate from your code.
We'll take a look at implementing a macro from scratch using the
[Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)
compiler plugin.

Let's implement a simpler syntax for declaring Algebraic Data Types (ADTs) in Scala.
I have an implementation of this on [GitHub][scala-adt-github],
but we'll target a simpler version for this exercise.

## Reviewing Algebraic Data Types

In Scala, we represent ADTs by way of subtype polymorphism (i.e. inheritance).

```scala
sealed trait Maybe[+A]
final case class Just[+A](a: A) extends Maybe[A]
case object Nix extends Maybe[Nothing]
```

Compare this with Haskell, which is significantly more terse than Scala -

```haskell
data Maybe a = Just a | Nix
```

Wouldn't it be nice if we could have something like the following in Scala
and get the same thing?

```scala
@ADT trait Maybe[A] { Just(a: A); Nix }
```

With macro annotations, this is possible! Note that macros, when applied to
classes, traits, or objects, can expand to a single class/trait/object or a
class/trait with a companion object. So let's try to target the following
output for our macro -

```scala
sealed trait Maybe[+A]
object Maybe {
  final case class Just[+A](a: A) extends Maybe[A]
  case object Nix extends Maybe[Nothing]
}
```

## Comprehension

A quick note on comprehending this tutorial. I strongly recommend actually
writing the code manually as you go along. Simply reading or pasting the code
won't help you gain the intuition needed as efficiently as typing the code yourself.
Enjoy!

## Dependencies

Let's get our **build.sbt** to match something like the following -

```scala
name := "scala-macros-intro"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
scalacOptions in Test ++= Seq("-Yrangepos")

crossScalaVersions := Seq(
  (2 to 6).map("2.10." + _),
  (0 to 8).map("2.11." + _)
).flatten

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
```

If there are newer versions available, you may wish to change the values above.

## Implementation

Let's start by stubbing out a macro definition.

**src/main/scala/intro/macros/ADT.scala**
```scala
package intro.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class ADT extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ADT.impl
}

object ADT {

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    ???
  }
}
```

In true TDD style, let's go ahead and create an accompanying test case -

**src/test/scala/intro/macros/ADTSpec.scala**
```scala
package intro.macros

import org.scalacheck.Properties

@ADT trait Maybe[A] { Just(a: A); Nix }

class ADTSpec extends Properties("@ADT macro") {

  // Import Just and Nix
  import Maybe._

  property("generates Just constructor") = {
    Just(0) == Just(0)
  }

  property("generates Nix singleton") = {
    Nix == Nix
  }
}
```

Let's start up SBT and confirm we get a failing test case -

```text
% sbt
> compile
[success]
> test
[error] ADTSpec.scala:5: exception during macro expansion:
[error] scala.NotImplementedError: an implementation is missing
...
```

We'll also get a bunch of other compiler errors as well. Let's focus on getting
compilable code to be generated from our macro and then we can confirm that the tests
pass.

Let's take another look at that `impl` method. Pretty much all of our macro logic
will reside in the body of this method.

```scala
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    ???
  }
```

There's a few things to note here. The `c` passed in is the macro `Context`.
This value contains all of the types and constructors required for building the
resulting [ASTs](https://en.wikipedia.org/wiki/Abstract_syntax_tree) wee need.


The second parameter is `annottees` which is the AST we have annotated. For instance,
our usage was the following -

```scala
@ADT trait Maybe[A] { Just(a: A); Nix }
```

So our `annottees` value will contain that entire `trait` definition. If we had
also defined a companion object, it would be included in the `annottees` value as well.

Finally, we return `c.universe.Tree`, which is just a resulting AST to replace our
input AST.

To get a better feel for what's going on here and how to actually deal with the
AST input, let's just print out the raw tree. For convenience, let's just import
everything from `c.universe` as it contains a bunch of stuff we'll need for
implementing our macro, including the `showRaw` function.

```scala
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._
    println(s"ANNOTTEES: ${showRaw(annottees)}")
    ???
  }
```

```text
> test
ANNOTTEES: List(Expr(ClassDef(Modifiers(ABSTRACT | DEFAULTPARAM/TRAIT), TypeName("Maybe"), List(TypeDef(Modifiers(PARAM), TypeName("A"), List(), TypeBoundsTree(EmptyTree, EmptyTree))), Template(List(Select(Ident(scala), TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(), TermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(())))), Apply(Ident(TermName("Just")), List(Typed(Ident(TermName("a")), Ident(TypeName("A"))))), Ident(TermName("Nix")))))))
```

Whoa, that's a little hard to look at. I've written a simple Haskell script called
[pprint-parens.hs](https://github.com/carymrobbins/dotfiles/blob/master/bin/pprint-parens.hs),
so feel free to use it while following along. Simply run the script and it will await
for text from stdin, so paste your raw AST and use `ctrl+d` to tell it you're done.
The tree above then becomes (showing just the first ten lines for brevity) -

```text
% pprint-parens > ast
<paste raw tree>
<press ctrl+d>
% head -n10 ast
```

```scala
List(
  Expr(
    ClassDef(
      Modifiers(ABSTRACT | DEFAULTPARAM/TRAIT), TypeName("Maybe"), List(
        TypeDef(
          Modifiers(PARAM), TypeName("A"), List(), TypeBoundsTree(
            EmptyTree, EmptyTree
          )
        )
      ), Template(
```

This may be a little intimidating, but the most important part is to
take note of the first few lines. Our `trait` is essentially represented as -

```scala
List(Expr(ClassDef(...)))
```

So let's match on that case, failing for other trees which we won't handle at the moment -

```scala
object ADT {

  val MACRO_NAME = "@ADT"

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.universe.Tree = {
    import c.universe._

    def fail(msg: String) = c.abort(c.enclosingPosition, msg)

    def run(): Tree = annottees match {
      // @ADT trait/class Foo { ... }
      case List(Expr(cls: ClassDef)) => ???

      case _ => fail(s"Invalid $MACRO_NAME usage")
    }

    run()
  }
}
```

You'll also notice that I'm defining `defs` within our `impl` method instead of
outside of it. The main reason for this is that it's much simpler to re-use the
names imported from `c.universe._`; otherwise, we'd have to pass `c` around to
all of the helper methods.

Now let's start deconstructing the AST and inspecting it. First, we'll
assert that the class we're dealing with is _actually_ a trait.

```scala
    def run(): Tree = annottees match {
      // @ADT trait Foo { ... }
      case List(Expr(cls: ClassDef)) => runClass(cls)

      case _ => fail(s"Invalid $MACRO_NAME usage")
    }

    def runClass(cls: ClassDef) = {
      val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls
      if (!clsMods.hasFlag(Flag.TRAIT)) fail(s"$MACRO_NAME requires trait")
      ???
    }
```

Now we're at the point where we'll need to inspect our input AST (which we printed
earlier) to see how our constructors (`Just(a: A)` and `Nix`) are appearing.

```scala
    def runClass(cls: ClassDef) = {
      val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls
      if (!clsMods.hasFlag(Flag.TRAIT)) fail(s"$MACRO_NAME requires trait")

      clsTemplate.body.zipWithIndex.foreach { case (part, i) =>
        println(s"part $i; ${showRaw(part)}")
      }
      ???
    }
```

```text
> test
part 0; DefDef(Modifiers(), TermName("$init$"), List(), List(List()), TypeTree(), Block(List(), Literal(Constant(()))))
part 1; Apply(Ident(TermName("Just")), List(Typed(Ident(TermName("a")), Ident(TypeName("A")))))
part 2; Ident(TermName("Nix"))
```

So there's a special `$init$` method, which we'll want to ignore, and then we have our
constructors. Scala sees the `Just(a: A)` as an `Apply` and the `Nix` as a simple `Ident`.
So we can just pick out the parts that we need.

```scala
    def runClass(cls: ClassDef) = {
      val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls
      if (!clsMods.hasFlag(Flag.TRAIT)) fail(s"$MACRO_NAME requires trait")

      val ctors = clsTemplate.body.collect {
        case Apply(Ident(name: TermName), args) => (name, args)
        case Ident(name: TermName) => (name, Nil)
      }

      ctors.foreach { case (name, args) => println(s"$name: ${showRaw(args)}") }

      ???
    }
```

```text
> test
Just: List(Typed(Ident(TermName("a")), Ident(TypeName("A"))))
Nix: List()
```

Ok, we see what we have to work with, so let's stub out our resulting
trait and companion object using _quasiquotes_ (`q"..."` syntax).
Quasiquotes essentially take Scala-like syntax inside of a quasiquote string
and generate an AST from it. This is particularly useful so you don't have to
manually construct all of the AST trees yourself. While there will be plenty
of times you _will_ need to construct ASTs manually, using quasiquotes makes the
entire process much simpler.

```scala
    def runClass(cls: ClassDef) = {
      val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls
      if (!clsMods.hasFlag(Flag.TRAIT)) fail(s"$MACRO_NAME requires trait")

      val ctors = clsTemplate.body.collect {
        case Apply(Ident(name: TermName), args) => (name, args)
        case Ident(name: TermName) => (name, Nil)
      }

      q"""
        sealed trait $clsName[..$clsParams]
        object $clsName {
          // TODO
        }
      """
    }
```

The `..$clsParams` syntax in the quasiquotes expands `List` values for you.
In this case, the `clsParams` is a `List[TypeDef]`, so it will automatically
turn that into a list of type parameters. The `..` technique in quasiquotes is
invaluable to making AST constructing less painful.

```text
> test
[error]  found   : c.universe.TypeName
[error]  required: c.universe.TermName
[error]         object $clsName {
[error]                 ^
```

Oops, it seems we need to use `TermName` for an `object` name.
Converting between `TypeName` and `TermName` is pretty simple; just use the
`.toTypeName` or `.toTermName` method.

```scala
      q"""
        sealed trait $clsName[..$clsParams]
        object ${clsName.toTermName} {
          // TODO
        }
      """
```

```text
> test
[error] ADTSpec.scala:10: not found: value Just
[error]     Just(0) == Just(0)
[error]     ^
```

Ok, this is good, we just need to actually generate the appropriate classes so we
have `Just` and `Nix` in scope.

If we look back at our input AST, we can see how the trait's type parameters are
represented -

```scala
        TypeDef(
          Modifiers(PARAM), TypeName("A"), List(), TypeBoundsTree(
            EmptyTree, EmptyTree
          )
        )
```

So we can get a list of the type parameters from the `clsParams` -

```scala
      val typeParams = clsParams.collect {
        case t: TypeDef if t.mods.hasFlag(Flag.PARAM) => t.name
      }
```

Now, before we actually start writing the code that needs to construct the
appropriate ASTs, we should probably know what we want our ASTs to look like.
Generally, the best way to do this is to use `scalac` to print the trees. So let's
write the following to a new file, **expected.scala**.
You can then use my
[scalac-ast](https://github.com/carymrobbins/dotfiles/blob/master/bin/scalac-ast)
script which simplifies printing trees with `scalac`.

```text
% cat expected.scala
sealed trait Maybe[+A]
object Maybe {
  final case class Just[+A](a: A) extends Maybe[A]
  case object Nix extends Maybe[Nothing]
}

% scalac-ast expected.scala > expected.ast

% pprint-parens.hs < expected.ast > expected.ast.pretty

% head -n10 expected.ast.pretty
[[syntax trees at end of parser]]// Scala source: expected.scala
PackageDef(
  Ident(
    TermName("<empty>")
  ), List(
    ClassDef(
      Modifiers(ABSTRACT | INTERFACE | SEALED | DEFAULTPARAM/TRAIT), TypeName(
        "Maybe"
      ), List(
        TypeDef(
```

If we look for `CASEACCESSOR`, we can find how to construct the appropriate AST
for case class arguments -

```scala
                ValDef(
                  Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName("a"), Ident(
                    TypeName("A")
                  ), EmptyTree
                ), DefDef(
```

Now we have enough information to write a function which can construct the
AST for a case class.

```scala
    def runClass(cls: ClassDef) = {
      val ClassDef(clsMods, clsName, clsParams, clsTemplate) = cls
      if (!clsMods.hasFlag(Flag.TRAIT)) fail(s"$MACRO_NAME requires trait")

      val typeParams = clsParams.collect {
        case t: TypeDef if t.mods.hasFlag(Flag.PARAM) => t.name
      }

      def mkCaseClass(name: TermName, args: List[Tree]) = {
        // Get the constructor arguments into a form we can deal with.
        val myArgs: List[(TermName, TypeName)] = args.map {
          case Typed(Ident(name: TermName), Ident(typ: TypeName)) => (name, typ)
          case other => fail(s"Unsupported constructor argument: $other")
        }
        // Keep track of the types used by those arguments so we can determine
        // which ones are used for extending the trait.
        val myArgTypes: List[TypeName] = myArgs.map(_._2)
        // If this case class uses one of the trait's type parameters, have the
        // case class extend from the trait using that type parameter. Otherwise,
        // use Nothing in that type parameter's place.
        val parentTypeParams = typeParams.map(
          p => if (myArgTypes.contains(p)) Ident(p) else Ident(TypeName("Nothing"))
        )
        // Construct the type parameters for this case class.
        val myTypeParams = myArgTypes.map(
          p => TypeDef(Modifiers(Flag.PARAM), p, Nil, TypeBoundsTree(EmptyTree, EmptyTree))
        )
        // Construct the arguments for this case class.
        val myCtorArgs = myArgs.map { case (argName, argType) =>
          ValDef(
            Modifiers(Flag.CASEACCESSOR | Flag.PARAMACCESSOR),
            argName, Ident(argType), EmptyTree
          )
        }
        q"""
            final case class ${name.toTypeName}[..$myTypeParams](..$myCtorArgs)
              extends $clsName[..$parentTypeParams]
        """
      }
```

Constructing an AST for a case object is pretty trivial -

```scala
      def mkCaseObject(name: TermName) = {
        val parentTypeParams = typeParams.map(_ => Ident(TypeName("Nothing")))
        q"case object $name extends $clsName[..$parentTypeParams]"
      }
```

Now we can just call the appropriate function to construct the right trees.

```scala
      val ctors = clsTemplate.body.collect {
        case Apply(Ident(name: TermName), args) => mkCaseClass(name, args)
        case Ident(name: TermName) => mkCaseObject(name)
      }

      q"""
        sealed trait $clsName[..$clsParams]
        object ${clsName.toTermName} {
          ..$ctors
        }
      """
    }
```

Whew, alright, let's run our test again.

```text
> test
[info] + @ADT macro.generates Nix singleton: OK, proved property.
[info] + @ADT macro.generates Just constructor: OK, proved property.
[info] Passed: Total 2, Failed 0, Errors 0, Passed 2
[success]
> 
```

## Congratulations!

We now have a working macro. However, this code is certainly not
perfect. What if you define methods in the trait? What if you define your own
companion object? Feel free to try these out yourself and see what happens. Spoiler,
it won't work as you'd want. I'd recommend digging into the [scala-adt][scala-adt-github]
source code to see how this is handled as well as some other features like
ADT enums.

[scala-adt-github]: https://github.com/carymrobbins/scala-adt
