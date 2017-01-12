module FunctorLaws
  ( functorComposition
  , functorIdentity
  )
where

functorComposition :: (Functor f, Eq (f c)) => f a -> (b -> c) -> (a -> b) -> Bool
functorComposition x f g = fmap (f . g) x == (fmap f . fmap g) x

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == id x
