# Chapter 21: Traversable

An extension of `Foldable` and `Functor`. The minimal methods of its
class definition summarise its purpose: evaluate and collect.

    class (Functor t, Foldable t) => Traversable t where

	    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
		traverse = sequenceA . fmap f

		sequenceA :: Applicative f => t (f a) -> f (t a)
		sequenceA = traverse id
