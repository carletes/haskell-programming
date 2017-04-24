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

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "Identity a" $ do
        let t = Identity (Sum 0 :: Sum Int)
            t3 = (t, (Sum 0, Sum 0, Sum 0) :: (Sum Int, Sum Int, Sum Int))
        testBatch $ traversable t3
