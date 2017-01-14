module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Constant a b

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant v) = Constant v

constIdentity :: Constant Int Int -> Bool
constIdentity = functorIdentity

constComposition :: Constant Int Int -> Fun Int Int -> Fun Int Int -> Bool
constComposition = functorComposition

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

--- Test suite driver

main :: IO ()
main = hspec $ do
    describe "The functor for Constant a b" $ do
        it "follows the identity law" $ do
            property constIdentity

        it "follows the composition law" $ do
            property constComposition
