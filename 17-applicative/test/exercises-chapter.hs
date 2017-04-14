{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Monoid
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write applicative instances for the following datatypes.

-- Pair a

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance Monoid (Pair Int) where
    mempty = Pair 0 0
    (Pair a b) `mappend` (Pair c d) = Pair (a + c) (b + d)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

-- Two a b (this is a tuple!)

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure b = Two mempty b
    (Two a f) <*> (Two a' b) = Two (a <> a') (f b )

instance Monoid (Two Int Int) where
   mempty = Two 0 0
   (Two a b) `mappend` (Two x y) = Two (a + x) (b + y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- Three a b c (a 3-tuple).

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance Monoid (Three Int Int Int) where
    mempty = Three 0 0 0
    (Three a b c) `mappend` (Three a' b' c') = Three (a+a') (b+b') (c+c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- Three' a b

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
    pure b = Three' mempty b b
    (Three' a f g) <*> (Three' a' x y) = Three' (a <> a') (f x) (g y)

instance Monoid (Three' Int Int) where
    mempty = Three' 0 0 0
    (Three' a b c) `mappend` (Three' a' b' c') = Three' (a+a') (b+b') (c+c')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

-- Four a b c d (a 4-tuple)

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance Monoid (Four Int Int Int Int) where
    mempty = Four 0 0 0 0
    (Four a b c d) `mappend` (Four a' b' c' d') = Four (a+a') (b+b') (c+c') (d+d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

-- Four' a b

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a1 a2 b) = Four' a a1 a2 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    (Four' a a1 a2 f) <*> (Four' a' a1' a2' x) = Four' (a <> a') (a1 <> a1') (a2 <> a2') (f x)

instance Monoid (Four' Int Int) where
    mempty = Four' 0 0 0 0
    (Four' a b c d) `mappend` (Four' a' b' c' d') = Four' (a+a') (b+b') (c+c') (d+d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Pair a" $ do
        testBatch $ monoid (undefined :: (Pair Int))
        testBatch $ functor (undefined :: (Pair Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Pair Int, (Int, Int, Int)))

    describe "Two a b" $ do
        testBatch $ monoid (undefined :: (Two Int Int))
        testBatch $ functor (undefined :: (Two Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Two Int Int, (Int, Int, Int)))

    describe "Three a b c" $ do
        testBatch $ monoid (undefined :: (Three Int Int Int))
        testBatch $ functor (undefined :: (Three Int Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Three Int Int Int, (Int, Int, Int)))

    describe "Three' a b" $ do
        testBatch $ monoid (undefined :: (Three' Int Int))
        testBatch $ functor (undefined :: (Three' Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Three' Int Int, (Int, Int, Int)))

    describe "Four a b c d" $ do
        testBatch $ monoid (undefined :: (Four Int Int Int Int))
        testBatch $ functor (undefined :: (Four Int Int Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Four Int Int Int Int, (Int, Int, Int)))

    describe "Four' a b" $ do
        testBatch $ monoid (undefined :: (Four' Int Int))
        testBatch $ functor (undefined :: (Four' Int Int, (Int, Int, Int)))
        testBatch $ applicative (undefined :: (Four' Int Int, (Int, Int, Int)))
