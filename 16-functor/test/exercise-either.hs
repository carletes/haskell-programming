module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Sum a

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a,
               return $ Second b]

sumIdentity :: Sum Int Int -> Bool
sumIdentity = functorIdentity

sumComposition :: Sum Int Int -> Fun Int Int -> Fun Int Int -> Bool
sumComposition = functorComposition

--- Let's go!

main :: IO ()
main = hspec $
    describe "Sum a b" $ do
        it "follows the identity law" $
            property sumIdentity

        it "follows the composition law" $
            property sumComposition
