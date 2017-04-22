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

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Two a b" $ do
        prop "is (,) a [1]" $
            ((\a b -> fold (a, b) == fold (Two a b)) :: Sum Int -> Sum Int -> Bool)

        prop "is (,) a [2]" $
            ((\a b -> fold (a, b) == fold (Two a b)) :: Product Int -> Product Int -> Bool)
