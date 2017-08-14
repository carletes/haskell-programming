{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Wrap f a

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

--- The kind of `Wrap` is interesting:
---
---     λ :k Wrap
---     Wrap :: (* -> *) -> * -> *
---     λ
---
--- We can see from `:k Wrap` that the kind of `f` is `* -> *` --- that is,
--- `f` might be a functor itself. An example:
---
---     λ :k (Wrap Maybe)
---     (Wrap Maybe) :: * -> *
---     λ
---
--- So, provided that `f` is a functor, we could define the functor instance
--- for `Wrap f`:

instance (Functor f) => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

--- Let's check that:

instance (Arbitrary a) => Arbitrary (Wrap [] a) where
    arbitrary = do
        a <- arbitrary
        return $ Wrap [a]

wrapIdentity :: Wrap [] Int -> Bool
wrapIdentity = functorIdentity

wrapComposition :: Wrap [] Int -> Fun [Int] [Int] -> Fun Int [Int] -> Bool
wrapComposition = functorComposition

main :: IO ()
main = hspec $
    describe "The functor for Wrap f a" $ do
        it "follows the identity law" $
            property wrapIdentity

        it "follows the composition law" $
            property wrapComposition
