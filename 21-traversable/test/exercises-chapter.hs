{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Monoid
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write `Traversable` instances fro the following data types:

-- Identity a

data Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f b (Identity a) = f a b

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance EqProp (Sum Int) where
    (=-=) = eq

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- Constant a b

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ c _ = c

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

-- Optional a

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
    pure = Yep
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    Yep f <*> Yep a = Yep (f a)

instance Foldable Optional where
    foldr _ b Nada = b
    foldr f b (Yep a) = f a b

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        oneof [return Nada, return $ Yep a]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

-- List a

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = (f <$> xs) `append` (fs <*> xs) where
      append :: List a -> List a -> List a
      append Nil ys = ys
      append (Cons x xs') ys = Cons x $ append xs' ys

instance Foldable List where
    foldr _ b Nil = b
    foldr f b (Cons x xs) = foldr f (f x b) xs

instance Traversable List where
    traverse f = foldr cons_f (pure Nil)
      where cons_f x xs = Cons <$> f x <*> xs

    sequenceA Nil = pure Nil
    sequenceA (Cons x xs) =  pure Cons <*> x <*> sequenceA xs

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Identity a" $ do
        let t = Identity (Sum 0 :: Sum Int)
            t3 = (t, (Sum 0, Sum 0, Sum 0) :: (Sum Int, Sum Int, Sum Int))
        testBatch $ traversable t3

    describe "Constant a b" $ do
        let t = Constant 0 :: Constant (Sum Int) (Sum Int)
            t3 = (t, (0, 0, 0)) :: (Constant (Sum Int) (Sum Int), (Sum Int, Sum Int, Sum Int))
        testBatch $ traversable t3

    describe "Optional a" $ do
        let t = Nada :: Optional (Sum Int)
            t3 = (t, (0, 0, 0)) :: (Optional (Sum Int), (Sum Int, Sum Int, Sum Int))
        testBatch $ traversable t3
