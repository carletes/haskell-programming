{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (Left, Right)

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write monad instances for the following types.
--
-- Nope a

data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    NopeDotJpg >>= _ = NopeDotJpg

instance Monoid (Nope Int) where
    mempty = NopeDotJpg
    _ `mappend` _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

-- PhhhbbtttEither b a (like `Either`, with error and result types flipped).

data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap f (Left a) = Left (f a)
    fmap _ (Right b) = Right b

instance (Monoid b) => Applicative (PhhhbbtttEither b) where
    pure a = Left a
    (Right _) <*> _ = Right mempty
    (Left _) <*> (Right b) = Right b
    (Left f) <*> (Left a) = Left (f a)

instance (Monoid b) => Monad (PhhhbbtttEither b) where
    (Right b) >>= _ = Right b
    (Left a) >>= f = f a

instance Monoid (PhhhbbtttEither Int Int) where
    mempty = Left 0
    (Right b) `mappend` _ = (Right b)
    (Left _) `mappend` (Right b) = Right b
    (Left a) `mappend` Left a' = Left (a+a')

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        b <- arbitrary
        a <- arbitrary
        oneof [return $ Left a, return $ Right b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

-- Identity a

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
    (Identity a) >>= f = f a

instance Monoid (Identity Int) where
    mempty = Identity 0
    (Identity a) `mappend` (Identity a') = Identity (a+a')

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq


-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Nope a" $ do
        testBatch $ monoid (undefined :: Nope Int)
        testBatch $ functor (undefined :: (Nope Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Nope Int, (Int, Int, Int)))
        testBatch $ monad (undefined :: (Nope Int, (Int, Int, Int)))
        testBatch $ monadFunctor (undefined :: (Nope Int, (Int, Int)))
        testBatch $ monadApplicative (undefined :: (Nope Int, (Int, Int)))

    describe "PhhhbbtttEither b a" $ do
        testBatch $ monoid (undefined :: (PhhhbbtttEither Int Int))
        testBatch $ functor (undefined :: (PhhhbbtttEither Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (PhhhbbtttEither Int Int, (Int, Int, Int)))
        testBatch $ monad (undefined :: (PhhhbbtttEither Int Int, (Int, Int, Int)))
        testBatch $ monadFunctor (undefined :: (PhhhbbtttEither Int Int, (Int, Int)))
        testBatch $ monadApplicative (undefined :: (PhhhbbtttEither Int Int, (Int, Int)))

    describe "Identity a" $ do
        testBatch $ monoid (undefined :: Identity Int)
        testBatch $ functor (undefined :: (Identity Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Identity Int, (Int, Int, Int)))
        testBatch $ monad (undefined :: (Identity Int, (Int, Int, Int)))
        testBatch $ monadFunctor (undefined :: (Identity Int, (Int, Int)))
        testBatch $ monadApplicative (undefined :: (Identity Int, (Int, Int)))
