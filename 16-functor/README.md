# Chapter 16: Functor

From the chapter introduction:

> `Functor` is all about a pattern of mapping over structure.

Later we learn that the structure is preserved, too.

The `Functor` typeclass is defined like this:

    class Functor f where
	  fmap :: (a -> b) -> f a -> f b

From the definition of `fmap`, it follows that the type `f` must have
a kind of `* -> *`.
