# Chapter 20: Foldable

Foldables are "data structures that can be folded to a summary value".

Here is a partial definition of `Foldable`:

    class Foldable t where
	  foldr :: (a -> b -> b) -> b -> t a -> b
	  fold :: Monoid m => t m -> m
	  foldMap :: Monoid m => (a -> m) -> t a -> m
	  ...

In `foldr` there is an implicit monoid defined inside the function
`(a -> b -> b)`. That implicit moinoid is explicit in `fold`. And in
`foldMap` the function `(a -> m)` explicitly maps each element of the
structure to a monoid.
