# Chapter 22: Reader

The definition of `Reader`:

    newtype Reader r a =
	  Reader { runReader :: r -> a }

`Reader` is a `Functor`:

    instace Functor (Reader r) where
	  fmap :: (a -> b) -> Reader r a -> Reader r b
	  fmap f (Reader ra) = Reader $ f . ra


And an `Applicative`, too --- see [exercise-reading-comprehension](test/exercise-reading-comprehension.hs)
