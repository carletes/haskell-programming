{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- The given definition of `List a`.

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- Helper function to create a `List a` from a regular Haskell list.

fromList :: [a] -> List a
fromList = foldr Cons Nil

-- The functor instance for `List` (required, since we want to define an
-- applicative instance).

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Our applicative instance for `List`.
--
-- The exercise requires it to be compatible with the default applicative
-- instance for regular Haskell lists.

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = (f <$> xs) `append` (fs <*> xs)

-- Some helper functions used above.

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

fold :: (a -> b -> b) -> b -> List a -> b
fold  _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ f <$> xs

-- QuickCheck boilerplate.

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = fromList <$> listOf arbitrary

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

-- There we go!

main :: IO ()
main = hspec $
    describe "List" $ do
        it "expected values" $
            fromList [(+1), (*2)] <*> fromList [1, 2]
                `shouldBe` fromList [2, 3, 2, 4]

        -- Functor laws.
        testBatch $ functor (undefined :: (List (Int, Int, Int)))

        -- Applicative laws.
        testBatch $ applicative (undefined :: (List (Int, Int, Int)))
