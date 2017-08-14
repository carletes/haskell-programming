{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Implement the `Either` monad.

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (First a) <*> _ = First a
    (Second _) <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= f = f b

-- Test suite helpers.

instance Monoid (Sum String Int) where
    mempty = Second 0
    (First a) `mappend` _ = First a
    (Second _) `mappend` (First a) = First a
    (Second b) `mappend` (Second b') = Second (b+b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ First a,
               return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

-- Test suite driver.

main :: IO ()
main = hspec $
    describe "Sum a b" $ do
        testBatch $ functor (undefined :: (Sum String Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Sum String Int, (Int, Int, Int)))
        testBatch $ monad (undefined :: (Sum String Int, (Int, Int, Int)))
        testBatch $ monadFunctor (undefined :: (Sum String Int, (Int, Int)))
        testBatch $ monadApplicative (undefined :: (Sum String Int, (Int, Int)))
