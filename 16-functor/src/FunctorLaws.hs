module FunctorLaws
  ( functorComposition
  , functorIdentity
  )
where

import Test.QuickCheck.Function

functorComposition :: (Functor f, Eq (f c)) => f a -> Fun b  c -> Fun a b -> Bool
functorComposition x (Fun _ f) (Fun _ g) = fmap (f . g) x == (fmap f . fmap g) x

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == id x
