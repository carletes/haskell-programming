# Chapter 18: Monad

Monads are introduced as applicatives with some extensions:

    class Applicative m => Monad m where
	  (>>=) :: m a -> (a -> m b) -> m b
	  (>>) :: m a -> m -> b -> m b
	  return :: a -> m a

Since monads are applicatives, they are functors, too. In fact the
following is required to hold:

    fmap f xs = xs >= return . f

The addition of monads regarding applicatives and funtors is
*partially* embedded in the `>>=` operator (the _bind operator_, as it
is called). Only partially, because `>>=` can be written in terms of
`fmap` and (here is the novelty) `join`:

    join :: Monad m => m (m a) -> m a

`join`, defined in `Control.Monad`, can be seen as an extension of
`concat`:

    concat :: Foldable t => t [a] -> [a]
