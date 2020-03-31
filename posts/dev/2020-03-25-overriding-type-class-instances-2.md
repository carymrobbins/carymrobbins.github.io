---
title: Overriding Type Class Instances (Part 2)
---

As we discovered in [part 1](../overriding-type-class-instances/), we can
build some infrastructure to override instances for a type class we've created.
But what if we want to do this for an existing type class? In most cases,
we likely won't be able to tamper with the source code to inject our
override machinery. Furthermore, we'd like to be able to achieve this with
as little boilerplate as possible.

## Spoiler Alert

The goal I set out to achieve was to end up with the following syntax.
Note that this allows us to override type class instances by
type or field name.

```haskell
import Data.Aeson (ToJSON)
import Data.Override (Override(Override), As)
import qualified Data.Override as Override

data MyRec = MyRec
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override MyRec
            '[ String `As` CharArray
             , "baz" `As` Uptext
             ]
```

The idea is that we can piggyback off of the existing generic machinery
used by `ToJSON`. We will use `DerivingVia` to derive an instance of
`ToJSON MyRec` via `Override MyRec` with a type-level list specifying the
type substitutions to make.

In our example, we are saying that when deriving a `ToJSON MyRec` instance,
for its fields, instead of `ToJSON String` use `ToJSON CharArray`
and, for the `baz` field, instead of `ToJSON Text` use `ToJSON Uptext`.
These alternate instances will be provided via newtype wrappers, something
we're likely all familiar with at this point.

```haskell
newtype Uptext = Uptext { unUptext :: Text }

instance ToJSON Uptext where
  toJSON = toJSON . Text.toUpper . unUptext

newtype CharArray = CharArray { unCharArray :: String }

instance ToJSON CharArray where
  toJSON = toJSON . map (:[]) . unCharArray
```

Now, via the derived `ToJSON MyRec` instance, we should be able to serialize this
Haskell value -

```haskell
MyRec
  { foo = 12
  , bar = "hi"
  , baz = "bye"
  }
```

into this JSON -

```json
{
  "foo": 12,
  "bar": ["h", "i"],
  "baz": "BYE"
}
```

Ok, now on to making this dream come true.

## The Approach

How can we possibly make this work for any type class? Well let us consider
how generic derivation works in the first place.

The `ToJSON` type class delegates to `genericToJSON` for its default
implementation -

```haskell
toJSON :: a -> Value
toJSON = genericToJSON defaultOptions
```

It's not too important to know the implementation of `genericToJSON` for
our uses, so we'll just worry about its type signature.

```haskell
genericToJSON
  :: (Generic a, GToJSON Value Zero (Rep a))
  => Options -> a -> Value
```

So it has its own generic version of `ToJSON` called `GToJSON` which operates on
`Rep` values. `Rep` is an associated type family introduced by the `Generic`
type class which tells us the generic representation of its type argument.

To give a concrete example, we can observe the generic representation type for
our `MyRec` type from earlier using `GHC.Generics.from` -

```haskell
% ghci
> :t from
from :: Generic a => a -> Rep a x

> :t from MyRec { foo = 1, bar = "hi", baz = "bye" }
from MyRec { foo = 1, bar = "hi", baz = "bye" }
  :: D1
       ('MetaData "MyRec" "Ghci1" "interactive" 'False)
       (C1
          ('MetaCons "MyRec" 'PrefixI 'True)
          (S1
             ('MetaSel
                ('Just "foo")
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy)
             (Rec0 Int)
           :*: (S1
                  ('MetaSel
                     ('Just "bar")
                     'NoSourceUnpackedness
                     'NoSourceStrictness
                     'DecidedLazy)
                  (Rec0 String)
                :*: S1
                      ('MetaSel
                         ('Just "baz")
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
                      (Rec0 Text))))
       x
```

A bit noisy, but you can see that the structure of our `MyRec` type is represented
here.

### Here's the important part!

What we can do is create our own type class that will produce a different
representation when we are passing things through our `Override` type. We can
replace the leaves of the generic representation with another type that will do
the override magic for us. We'll call this type `Overridden`.

```haskell
% ghci
> :{
   r :: Override MyRec '[String `As` CharArray]
   r = Override MyRec { foo = 1, bar = "hi", baz = "bye" }
  :}

> :t from r
from r
  :: M1
       D
       ('MetaData "MyRec" "Ghci1" "interactive" 'False)
       (M1
          C
          ('MetaCons "MyRec" 'PrefixI 'True)
          (M1
             S
             ('MetaSel
                ('Just "foo")
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy)
             (K1 R (Overridden ('Just "foo") Int '[As String CharArray]))
           :*: (M1
                  S
                  ('MetaSel
                     ('Just "bar")
                     'NoSourceUnpackedness
                     'NoSourceStrictness
                     'DecidedLazy)
                  (K1 R (Overridden ('Just "bar") [Char] '[As String CharArray]))
                :*: M1
                      S
                      ('MetaSel
                         ('Just "baz")
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
                      (K1 R (Overridden ('Just "baz") Text '[As String CharArray])))))
       x
```

As you can see, the leaf nodes of our generic representation have now been
tagged as `Overridden` and include the field name, underlying type, and
type-level list of overrides.

## Making it really work for real

First, we'll need to define the types needed for this syntax.

The _entry point_ for our generic override machinery will be the `Override`
newtype.

```haskell
newtype Override a (xs :: [*]) = Override { unOverride :: a }
```

This takes two type parameters -

* `a` - The type which contains fields for which we will be overriding instances
        during generic derivation
* `xs` - A type-level list of overrides

Next, we need to define the `As` type which can be conveniently used infix via
`TypeOperators`. This is similar to how it was defined in part 1 except this time
it uses `PolyKinds` so we can target either a concrete type or a field name to
override.

```haskell
data As (o :: k) n
```

This takes two type parameters -

* `o` - The target to be overridden. Should be either the concrete type to
        or field name to override.
* `n` - The type to replace `o` with.

Now we'll create the `Overridden` type used at the leaf nodes of the generic
representation of an `Override` type.

```haskell
newtype Overridden (ms :: Maybe Symbol) a (xs :: [*]) =
  Overridden { unOverridden :: a }
```

This has the same type parameters as `Override` except for `ms`. The `ms` holds
an optional type-level string which, when available, will contain the field name
for a leaf node we can override.

We'll also need to introduce a type family called `Using` which will be used to
"pick" which instance to use from a list of overrides.

```haskell
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))

type family Using (ms :: Maybe Symbol) (x :: *) (xs :: [*]) where
  -- No matching override found.
  Using ms x '[] = x

  -- Override the matching field.
  Using ms x (As (o :: Symbol) n ': xs) =
    If (ms == 'Just o) n (Using ms x xs)

  -- Override the matching type.
  Using ms x (As (o :: *) n ': xs) =
    If (x == o) n (Using ms x xs)
```

The use of `PolyKinds` with our `As` type allows us to override by `Symbol`
or concrete type. The `ms` and `x` supplied to `Using` will be the same
`Maybe Symbol` and `a` as from `Overridden`. This allows us to conditionally
match on the field name or type.

### GOverride

In order for us to build our own `Rep` for our `Override` type (instead of
relying on the one provided by GHC) we'll need to introduce a new type class.

```haskell
class GOverride (xs :: [*]) (f :: * -> *) where
  type OverrideRep xs f :: * -> *
  overrideFrom :: f x -> OverrideRep xs f x
  overrideTo :: OverrideRep xs f x -> f x
```

The `OverrideRep` is analogous to the `Rep` from `GHC.Generics`, except it takes
a `Rep` and produces a new one which has `Overridden` injected at its leaves.
The same goes for `overrideFrom` and `overrideTo`, which are analogous to `from`
and `to` from `GHC.Generics` but perform runtime injection of `Overridden` at
the leaves of the generic representation.

We can then define the instance for `Generic Override` as follows, delegating to
our `GOverride` type class.

```haskell
instance (Generic a, GOverride xs (Rep a)) => Generic (Override a xs) where
  type Rep (Override a xs) = OverrideRep xs (Rep a)
  from = overrideFrom @xs . from . unOverride
  to = Override . to . overrideTo @xs
```

We can then define instances for `GOverride` which operate on the generic
representation nodes -

```haskell
instance (GOverride xs f) => GOverride xs (M1 D c f) where
  type OverrideRep xs (M1 D c f) = M1 D c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f) => GOverride xs (M1 C c f) where
  type OverrideRep xs (M1 C c f) = M1 C c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f, GOverride xs g) => GOverride xs (f :*: g) where
  type OverrideRep xs (f :*: g) = OverrideRep xs f :*: OverrideRep xs g
  overrideFrom (f :*: g) = overrideFrom @xs f :*: overrideFrom @xs g
  overrideTo (f :*: g) = overrideTo @xs f :*: overrideTo @xs g

-- Instance for selecting a field.
-- * 'ms' is a 'Maybe Symbol' containing the field name, if applicable.
-- * 'su' is unused but passed through; short for 'SourceUnpackedness'
-- * 'ss' is unused but passed through; short for 'SourceStrictness'
-- * 'ds' is unused but passed through; short for 'DecidedStrictness'
-- * 'c' is the type of the field.
instance GOverride xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) where
  type OverrideRep xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) =
    M1 S ('MetaSel ms su ss ds) (K1 R (Overridden ms c xs))
  overrideFrom (M1 (K1 x)) = M1 (K1 (Overridden @ms x))
  overrideTo (M1 (K1 (Overridden x))) = M1 (K1 x)
```

That last instance is probably the most important. We extract both the `M1 S`
and `K1` so we can supply the optional field name `ms` and field type `c` to
`Overridden`.

## Aeson Glue

Now we'll talk about writing _glue code_ which binds our generic override
machinery to the aeson library, specifically, the `ToJSON` type class.

As previously mentioned, `ToJSON` uses `genericToJSON` for generic derivation -

```haskell
genericToJSON
  :: (Generic a, GToJSON Value Zero (Rep a))
  => Options -> a -> Value
```

It has another function, `toEncoding`, which is generically derived with,
you guessed it, `genericToEncoding` -

```haskell
genericToEncoding
  :: (Generic a, GToJSON Encoding Zero (Rep a))
  => Options -> a -> Encoding
```

So we can now write an instance for `ToJSON Override` which leverages
the default implementations for `ToJSON` -

```haskell
import qualified Data.Aeson as Aeson

instance
  ( Generic (Override a xs)
  , Aeson.GToJSON Aeson.Zero (Rep (Override a xs))
  , Aeson.GToEncoding Aeson.Zero (Rep (Override a xs))
  ) => Aeson.ToJSON (Override a xs)
```

In this case, we don't have to implement the `toJSON` and `toEncoding` methods;
the default implementations are exactly what we would have written.

Next we'll need to implement the instance for the `Overridden` type which was
injected at the leaves of our generic representation.

```haskell
instance
  ( Coercible a (Using ms a xs)
  , Aeson.ToJSON (Using ms a xs)
  ) => Aeson.ToJSON (Overridden ms a xs)
  where
  toJSON = Aeson.toJSON @(Using ms a xs) . coerce
  toEncoding = Aeson.toEncoding @(Using ms a xs) . coerce
```

And that's it! We now have generic override support for `ToJSON`!

## Exploring Overrides

Let's revisit our original `MyRec` declaration.

```haskell
data MyRec = MyRec
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override MyRec
            '[ String `As` CharArray
             , "baz" `As` Uptext
             ]
```

Now let's print out its JSON representation.

```haskell
% ghci
> enc = Data.ByteString.Lazy.Char8.putStrLn . Data.Aeson.Encode.Pretty.encodePretty
> enc MyRec { foo = 1, bar = "hi", baz = "bye" }
{
    "foo": 1,
    "baz": "BYE",
    "bar": [
        "h",
        "i"
    ]
}
```

Nice, it works!

Let's explore the possibilities for a moment using a function for easily
creating overrides on the fly.

```haskell
override :: a -> proxy xs -> Override a xs
override a _ = Override a
```

This allows us to specify ad hoc overrides as a `proxy` and summon the
appropriate instances as such. Let's try it out!

```haskell
% ghci
> :{
    data MyRec2 = MyRec2
      { foo :: String
      , bar :: String
      , baz :: String
      } deriving stock (Show, Eq, Generic)
  :}

> r = MyRec2 { foo = "one", bar = "hi", baz = "bye" }
```

No overrides will give us the default generically derived instance.

```haskell
> enc $ override r $ Proxy @'[]
{
    "foo": "one",
    "baz": "bye",
    "bar": "hi"
}
```

We can specify a single override as before.

```haskell
> enc $ override r $ Proxy @'[String `As` CharArray]
{
    "foo": [
        "o",
        "n",
        "e"
    ],
    "baz": [
        "b",
        "y",
        "e"
    ],
    "bar": [
        "h",
        "i"
    ]
}
```

We can also specify an override for a single field and a default for all other
fields matching a type.

```haskell
> :{
    newtype Upstring = Upstring { unUpstring :: String }

    instance ToJSON Upstring where
      toJSON = toJSON . map Char.toUpper . unUpstring
  :}

> enc $ override r $ Proxy @'["bar" `As` CharArray, String `As` Upstring]
{
    "foo": "ONE",
    "baz": "BYE",
    "bar": [
        "h",
        "i"
    ]
}
```

We can even tell a field to use its original instance, overriding the override!

```haskell
> enc $ override r $ Proxy @'["baz" `As` String, String `As` Upstring]
{
    "foo": "ONE",
    "baz": "bye",
    "bar": "HI"
}
```

What if we get the types wrong? Is it safe? Yes!

```haskell
> enc $ override r $ Proxy @'[String `As` Uptext]
```
```text
<interactive>:121:1: error:
    • Couldn't match representation of type ‘Text’
                               with that of ‘[Char]’
        arising from a use of ‘enc’
    • In the expression:
        enc $ override r $ Proxy @'[String `As` Uptext]
      In an equation for ‘it’:
          it = enc $ override r $ Proxy @'[String `As` Uptext]
```

This also works for fields just as well.

```haskell
> enc $ override r $ Proxy @'["bar" `As` Uptext]
```
```text
<interactive>:122:1: error:
    • Couldn't match representation of type ‘Text’
                               with that of ‘[Char]’
        arising from a use of ‘enc’
    • In the expression: enc $ override r $ Proxy @'["bar" `As` Uptext]
      In an equation for ‘it’:
          it = enc $ override r $ Proxy @'["bar" `As` Uptext]
```

There is one gotcha, however, and that is that you will want to supply field
overrides _first_ and type overrides _last_.

```haskell
> enc $ override r $ Proxy @'[String `As` Upstring, "bar" `As` CharArray]
{
    "foo": "ONE",
    "baz": "BYE",
    "bar": "HI"
}
```

For those in which "just get it right" is not an acceptable solution, we could
devise some sort of `ValidateOverride` type class that, upon attempting to do
any sort of overriding, checks for situations like this and reports them as
compiler errors. Or we could specialize the `Using` type family to prefer
fields over types in overrides.

However, I think it's always a good idea to test your codecs and instances
anyway, especially when doing any sort of specialization, so until such
machinery were implemented this is probably an acceptable gotcha to be aware of.

## That's it for now

There's still likely more exploration and work to be done in this area, but so
far I've packaged this up into a workable library with tests and more examples.

**GitHub**
* [generic-override](https://github.com/estatico/generic-override/)
* `generic-override-aeson` is included in the above repo.

**Hackage**
* [generic-override](https://hackage.haskell.org/package/generic-override)
* [generic-override-aeson](https://hackage.haskell.org/package/generic-override-aeson)
