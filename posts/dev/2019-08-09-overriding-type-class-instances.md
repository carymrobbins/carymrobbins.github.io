---
title: Overriding Type Class Instances (Part 1)
---

It's well known in the Haskell world that type class instances _cannot_
be overridden. When you have an instance in scope, you are stuck with
it. Is this a feature? Is this a bug? Maybe either depending on the
problem you are facing. I have my own opinions, but let me lay out
the case for wanting to be able to override instances.

<div class="alert alert-warning">
**Disclaimer:** This idea is quite experimental and not entirely fleshed
out. Consider this as purely a proof-of-concept that is clearly lacking
features and could potentially be refined and simplified. There's also the
chance that this has already been invented, possibly in a better form, I just
have yet to come across it. That being said, **you have been warned**.
</div>

Consider the following _very simple_ type class for encoding
values to `Text` -

```haskell
class Encode a where
  encode :: a -> Text
```

We'll define some instances for it -

```haskell
instance Encode Int where
  encode = Text.pack . show

instance Encode Text where
  encode = id
```

and they'll do what you expect -

```haskell
% ghci
> encode (12 :: Int)
"12"
> encode ("foo" :: Text)
"foo"
```

Now let's say we want to derive an `Encode` instance for
a record type -

```haskell
data Rec = Rec
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (Encode) via GenericEncode Rec
```

We'll derive `Generic` and use a newtype `GenericEncode`
(that we haven't written yet)
along with `DerivingVia` to use the generically-derived instance
for `Encode`.

What we'll want is this behavior -

```haskell
% ghci
> encode (Rec 12 "hi" "bye")
"foo = 12, bar = hi, baz = bye"
```

Going over generic derivation should be saved for another post, but I'll
quickly show the code and touch on the relevant parts.

```haskell
newtype GenericEncode a = GenericEncode { unGenericEncode :: a }

instance (Generic a, Encode (Rep a p)) => Encode (GenericEncode a) where
  encode = (encode @(Rep a p)) . from . unGenericEncode

instance (Encode (f p)) => Encode (M1 D x f p) where
  encode (M1 x) = encode @(f p) x

instance (Encode (f p)) => Encode (M1 C x f p) where
  encode (M1 x) = encode @(f p) x

instance (Encode (a p), Encode (b p)) => Encode ((a :*: b) p) where
  encode (a :*: b) = (encode @(a p) a) <> ", " <> (encode @(b p) b)

instance (Encode t, Selector s) => Encode (M1 S s (K1 R t) p) where
  encode m@(M1 (K1 x)) = Text.pack (selName m) <> " = " <> encode x
```

Now this _just works_. Trust me, I swear.

Ok ok, this is all well and good, but let's say we don't like
the `Encode Text` instance. It's fine in most cases, but our
`Rec` type needs to use a different instance. Do we have to give up
on generic derivation and write our `Encode Rec` instance from scratch?

Maybe not! Let's play with an idea.

Here's the new `Text` instance we want to use. We'll be resorting to
the classic _newtype trick_.

```haskell
newtype Uptext = Uptext { unUptext :: Text }

instance Encode Uptext where
  encode = Text.toUpper
```

Yes, it's contrived, but you really don't want me going into the _actual_ use
case for this. It's much messier and this is just easier to think about.

With `DerivingVia`, we could possibly do something like this -

```haskell
deriving via Encode Uptext instance Encode Text
```

This would work, except -
1. It requires that an `Encode Text` instance does not already exist
2. Now everyone has to use our instance, and for our purposes we only
   want `Rec` to use it.

Maybe we can somehow embed the instance we want to override directly into the
deriving clause. Hmm, how about something like -

```haskell
data Rec = Rec
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (Encode)
      via GenericEncode Rec
        `Using` Encode (Text `As` Uptext)
```

Looks a little funny, but ignoring the clumsiness (and limitations), can this
even work?

First, let's create some types to get this thing rolling -

```haskell
newtype a `Using` (x :: Constraint) = Using a
  deriving stock (Show)

data a `As` b = As
  deriving stock (Show)
```

So we'll use `Using` as a way to embed our overriding constraint into the type
of the value we want to encode, and `As` will be used to tell the instance
derivation which instance should be replaced and by which.

We can even use these types directly in the repl. Here's the value we'll want to
be dealing with -


```haskell
% ghci
> :{
    Using $ GenericEncode $ Rec 12 "hi" "bye"
      :: GenericEncode Rec `Using` Encode (Text `As` Uptext)
  :}
Using (GenericEncode {unGenericEncode = Rec {foo = 12, bar = "hi", baz = "bye"}})
```

At this point, we can use virtually our same generic deriving machinery, just
with `Using` sprinkled throughout to keep track of our overridden instances.
I won't copy pasta all of the boilerplate here (as it's just uglier versions
of the instances you've already seen). Instead, I'll point out the most
important bits.

The "entry point" instance to our generic derivation is almost exactly the same
as before, except, as mentioned, with `Using` stuff sprinkled in everywhere.
We're actually going to rip the `Using` constraint bits off of the passed in
type and apply it to the generic representation `Rep a p`, and then keep
applying it on each of the field selectors so we can pick the right instance.

```haskell
instance
  ( Generic a
  , Encode (Rep a p `Using` Encode (c `As` d))
  ) => Encode (GenericEncode a `Using` Encode (c `As` d)) where
  encode (Using x) =
    (encode @(Rep a p `Using` Encode (c `As` d)))
      $ Using @(Rep a p) @(Encode (c `As` d))
      $ from @a
      $ unGenericEncode x
```

So aside from sprinkling in `Using` everywhere, we'll need to change the
instance that actually does the encoding for each field.

```haskell
instance {-# OVERLAPPING #-}
  ( Encode b
  , Coercible a b
  ) => Encode (a `Using` Encode (a `As` b))
  where
  encode (Using a) = encode @b (coerce a)
```

Now what we have is an instance that works for a given type `a` when we want to
override it with the instance for type `b`. And of course, this only works if
`a` is `Coercible` to `b`.

We can even play with this directly!

```haskell
> encode (Using "hi" :: Text `Using` Encode (Text `As` Uptext))
"HI"
```

That's actually neat on its own.

Note that we need to make this an `OVERLAPPING`
instance so we can default to a different instance when this one doesn't apply.
Leaving it as it is would give us this when using a non-`Text` type -

```haskell
> encode (Using 1 :: Int `Using` Encode (Text `As` Uptext))
error:
    • No instance for (Encode (Using Int (Encode (As Text Uptext))))
```

Now we'll define our "default" instance that works when the other doesn't match -

```haskell
instance {-# OVERLAPPABLE #-}
  (Encode c) => Encode (c `Using` Encode (a `As` b))
  where
  encode (Using c) = encode @c c
```

There's actually a gotcha here, but I'll leave that as an exercise for the
reader.

Moving blissfully along, now the following works -

```haskell
> encode (Using 1 :: Int `Using` Encode (Text `As` Uptext))
"1"
```

Ok, so now let's review our previous deriving hackery -

```haskell
data Rec = Rec
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (Encode)
      via GenericEncode Rec `Using` Encode (Text `As` Uptext)
```

...but does it work?

```haskell
> encode $ Rec 12 "hi" "bye"
"foo = 12, bar = HI, baz = BYE"
```

Eureka! It does!

Is any of this practical? Maybe, maybe not. One important limitation here is that
you need to write your generic deriving machinery to deal with this. Also, this
implementation doesn't deal with _multiple_ overrides, it only supports one.
However, this could likely be solved without too much effort (type-level lists
are the first to come to mind).

But I think this is a good starting point. From here, we can potentially refine this
approach and make it a bit more versatile. It would be pretty exciting to
modify it in such a way that _doesn't_ require the deriving machinery to be
aware of our `Using` and `As` types. I have some ideas on how to make this a
reality, but there's more experimentin' to do first!

The actual working code for this post can be found [here](https://github.com/carymrobbins/scoped-instances).
