module Main where

import Test.Hspec
import Test.QuickCheck

import FunctorLaws

--- An incorecctly-defined functor

data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

instance Functor CountingBad where
    --- The `(n + 1)` is what breaks the functor identity law.
    fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)

--- A correctly-defined functor instance

data CountingGood a =
    Heisenberg' Int a
    deriving (Eq, Show)

instance Functor CountingGood where
    fmap f (Heisenberg' n a) = Heisenberg' n (f a)

--- QuickCheck boilerplate

instance (Arbitrary a) => Arbitrary (CountingBad a) where
    arbitrary = do
        a <- arbitrary
        n <- arbitrary
        return (Heisenberg n a)

noFunctorIdentity :: CountingBad Int -> Bool
noFunctorIdentity = not . functorIdentity

instance (Arbitrary a) => Arbitrary (CountingGood a) where
    arbitrary = do
        a <- arbitrary
        n <- arbitrary
        return (Heisenberg' n a)

followsFunctorIdentity :: CountingGood Int -> Bool
followsFunctorIdentity = functorIdentity

main :: IO ()
main = hspec $ do
    describe "Functor for `CountingBad`" $
        it "breaks the functor identity law" $
            property noFunctorIdentity

    describe "Functor for `CountingGood`" $
        it "follows the functor identity law" $
            property followsFunctorIdentity
