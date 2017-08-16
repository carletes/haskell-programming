{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import Prelude                  hiding (Left, Right)

import Test.Hspec
import Test.Hspec.Checkers
import Test.Hspec.QuickCheck
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
    fmap f (Left x)  = Left (f x)
    fmap _ (Right y) = Right y

instance (Monoid b) => Applicative (PhhhbbtttEither b) where
    pure = Left
    (Right _) <*> _ = Right mempty
    (Left _) <*> (Right y) = Right y
    (Left f) <*> (Left x) = Left (f x)

instance (Monoid b) => Monad (PhhhbbtttEither b) where
    (Right y) >>= _ = Right y
    (Left x) >>= f = f x

instance Monoid Int where
    mempty = 0
    x `mappend` y = x + y

instance Monoid (PhhhbbtttEither Int Int) where
    mempty = Left 0
    (Right y) `mappend` _ = (Right y)
    (Left _) `mappend` (Right y) = Right y
    (Left x) `mappend` Left x' = Left (x+x')

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        y <- arbitrary
        x <- arbitrary
        oneof [return $ Left x, return $ Right y]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

-- Identity a

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
    (Identity x) >>= f = f x

instance Monoid (Identity Int) where
    mempty = Identity 0
    (Identity x) `mappend` (Identity x') = Identity (x+x')

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

-- List a

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` xs = xs
    xs `mappend` Nil = xs
    (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

instance Monad List where
    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x `mappend` join (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        oneof [return Nil,
               return $ Cons x xs]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

-- Write the following functions using the methods provided by `Monad` and `Functor`.

j :: Monad m => m (m a) -> m a
j x = do
    y <- x
    y

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = do
    a' <- ma
    return $ f a'

l1' :: Monad m => (a -> b) -> m a -> m b
l1' f ma = ma >>= (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
    a' <- ma
    b' <- mb
    return $ f a' b'

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
    f <- mf
    fmap f ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
    fx <- f x
    fxs <- meh xs f
    return $ fx : fxs

flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id


-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Nope a" $ do
        let t = NopeDotJpg :: Nope Int
            t2 = (NopeDotJpg, (0, 0)) :: (Nope Int, (Int, Int))
            t3 = (NopeDotJpg, (0, 0, 0)) :: (Nope Int, (Int, Int, Int))
        testBatch $ monoid t
        testBatch $ functor t3
        testBatch $ applicative t3

        it "implements the `Monad` typeclass" $ do
            let f _ = NopeDotJpg
            (NopeDotJpg >>= f) `shouldBe` NopeDotJpg

        testBatch $ monad t3
        testBatch $ monadFunctor t2
        testBatch $ monadApplicative t2

    describe "PhhhbbtttEither b a" $ do
        let t = Left 0 :: PhhhbbtttEither Int Int
            t2 = (t, (0, 0)) :: (PhhhbbtttEither Int Int, (Int, Int))
            t3 = (t, (0, 0, 0)) :: (PhhhbbtttEither Int Int, (Int, Int, Int))
        testBatch $ monoid t
        testBatch $ functor t3
        testBatch $ applicative t3

        it "implements the `Monad` typeclass" $ do
            let plusOne x = Left (x + 1)
            (Left 41 >>= plusOne) `shouldBe` (Left 42 :: PhhhbbtttEither Int Int)
            (Right 41 >>= plusOne) `shouldBe` (Right 41 :: PhhhbbtttEither Int Int)

        testBatch $ monad t3
        testBatch $ monadFunctor t2
        testBatch $ monadApplicative t2

    describe "Identity a" $ do
        let t = Identity 0
            t2 = (t, (0, 0)) :: (Identity Int, (Int, Int))
            t3 = (t, (0, 0, 0)) :: (Identity Int, (Int, Int, Int))

        testBatch $ monoid t
        testBatch $ functor t3
        testBatch $ applicative t3

        it "implements the `Monad` typeclass" $ do
            let plusOne x = Identity (x + 1)
            (Identity 41 >>= plusOne) `shouldBe` (Identity 42 :: Identity Int)

        testBatch $ monad t3
        testBatch $ monadFunctor t2
        testBatch $ monadApplicative t2

    describe "List a" $ do
        let t = Nil :: List Int
            t2 = (t, (0, 0)) :: (List Int, (Int, Int))
            t3 = (t, (0, 0, 0)) :: (List Int, (Int, Int, Int))

        testBatch $ monoid t
        testBatch $ functor t3
        testBatch $ applicative t3

        it "implements the `Monad` typeclass" $ do
            let plusOne x = Cons (x+1) Nil :: List Int
            (Nil >>= plusOne) `shouldBe` Nil
            (Cons 41 (Cons 42 (Cons 43 Nil)) >>= plusOne) `shouldBe` Cons 42 (Cons 43 (Cons 44 Nil))

        testBatch $ monad t3
        testBatch $ monadFunctor t2
        testBatch $ monadApplicative t2

    describe "j" $ do
        prop "is `join` (1)" ((\xs -> j xs == join xs) :: ([[Int]] -> Bool))
        prop "is `join` (2)" ((\xs -> j xs == join xs) :: (Maybe (Maybe Int) -> Bool))

    describe "l1" $ do
        let f = (1+)
        prop "is liftM (1)" ((\xs -> l1 f xs == liftM f xs) :: ([Int] -> Bool))
        prop "is liftM (2)" ((\xs -> l1 f xs == liftM f xs) :: (Maybe Int -> Bool))

    describe "l1'" $ do
        let f = (1+)
        prop "is liftM (1)" ((\xs -> l1' f xs == liftM f xs) :: ([Int] -> Bool))
        prop "is liftM (2)" ((\xs -> l1' f xs == liftM f xs) :: (Maybe Int -> Bool))

    describe "l2" $ do
        let f = (+)
        prop "is liftM2 (1)"
            ((\xs ys -> l2 f xs ys == liftM2 f xs ys) :: ([Int] -> [Int]-> Bool))
        prop "is liftM2 (2)"
            ((\xs ys -> l2 f xs ys == liftM2 f xs ys) :: (Maybe Int -> Maybe Int -> Bool))

    describe "a" $ do
        let f = [(1+)]
        prop "is ap flipped (1)"
            ((\xs -> xs `a` f == f `ap` xs) :: [Int] -> Bool)

    describe "meh" $ do
        let f x = if x == 42 then Just 42 else Nothing :: Maybe Int
        prop "is forM"
            ((\xs -> meh xs f == forM xs f) :: [Int] -> Bool)

    describe "flipType" $
        prop "is `sequence`"
            ((\xs -> flipType xs == sequence xs) :: [Maybe Int] -> Bool)
