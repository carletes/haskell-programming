module Main where

import Data.Foldable
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck

-- Write `Foldable` instances for the following data types:

-- Constant a b

data Constant a b = Constant a
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr _ b _ = b

-- Two a b

data Two a b = Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f c (Two _ b) = f b c

-- Three a b c

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f d (Three _ _ c) = f c d

-- Three' a b

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f c (Three' _ b _) = f b c

--- Write `filterF` using `foldMap`

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p as = foldMap (\a -> if p a then pure a else mempty) as

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Two a b" $ do
        prop "is (,) a [1]" $
            ((\a b -> fold (a, b) == fold (Two a b)) :: Sum Int -> Sum Int -> Bool)

        prop "is (,) a [2]" $
            ((\a b -> fold (a, b) == fold (Two a b)) :: Product Int -> Product Int -> Bool)

    describe "Three a b c" $ do
        prop "is folded into the last component [1]" $
            ((\a b c -> fold (Three a b c) == c) :: Sum Int -> Sum Int -> Sum Int -> Bool)

        prop "is folded into the last component [2]" $
            ((\a b c -> fold (Three a b c) == c) :: Product Int -> Product Int -> Product Int -> Bool)

    describe "Three' a b c" $ do
        prop "is folded into the second component [1]" $
            ((\a b c -> fold (Three' a b c) == b) :: Sum Int -> Sum Int -> Sum Int -> Bool)

        prop "is folded into the second component [2]" $
            ((\a b c -> fold (Three' a b c) == b) :: Product Int -> Product Int -> Product Int -> Bool)

    describe "filterF" $ do
        it "filters and folds" $ do
            (filterF (even . getSum) [1, 2, 3, 4] :: Maybe (Sum Int)) `shouldBe` Just 6
            (filterF (even . getSum) [1, 3, 5, 7] :: Maybe (Sum Int)) `shouldBe` Nothing
