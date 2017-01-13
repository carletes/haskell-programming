module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Identity a

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

identityIdentity :: Identity Int -> Bool
identityIdentity = functorIdentity

identityComposition :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool
identityComposition = functorComposition

-- Pair a a

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a

pairIdentity :: Pair Int -> Bool
pairIdentity = functorIdentity

pairComposition :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool
pairComposition = functorComposition

--- Let's go!

main :: IO ()
main = hspec $ do
    describe "Identity a" $ do
        it "follows the identity law" $ do
            property identityIdentity

        it "follows the composition law" $ do
            property identityComposition

    describe "Pair a" $ do
        it "follows the identity law" $ do
            property pairIdentity

        it "follows the composition law" $ do
            property pairComposition
