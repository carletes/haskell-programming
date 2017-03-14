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
