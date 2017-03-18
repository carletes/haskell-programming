# Chapter 17: Applicative

Applicatives are monoidal functors. From the introduction:

> But with Applicative the function we’re applying is also embedded in
> some structure.  Because the function and the value it’s being
> applied to both have structure, we have to smash those structures
> together. So, Applicative involves monoids and functors.

The definition of `Applicative` requires `Functor`:

    class Functor f => Applicative f where
	  pure :: a -> f a
	  (<*>) :: f (a -> b) -> f a -> f b

Our trip to `Applicative` started with simple function application:

    ($) :: (a -> b) -> a -> b

We then discovered `Functor`, which lets us map a function over some
kind of structure, in a way that preserves structure:

    (<$>) :: (a -> b) -> f a -> f b

`Applicative` takes this a step further, by letting us apply functions
which are embedded _in the same type of structure_:

    (<*>): f (a -> b) -> f a -> f b


## What "monoidal" means in "monoidal functor"

Back to `ap` (another name for `<*>`):

    (<*>) :: f (a -> b) -> f a -> f b

We started with:

    ($) :: (a -> b) -> a -> b

So `($)` gives us, in a sense, a way of smashing together a function
of type `(a -> b)` with a value of type `a`. The result is a value of
type `b`.

What we want, however, is a way of smashing together something of type
`f (a -> b)` with a value of type `f a`, giving us something else of
type `f b`. In a way that the structure (the `f`) is preserved.

That is precisely what `Monoid`s do:

    mappend :: f -> f -> f
