module Main where

import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck

-- Implement the functions in terms of `foldMap` or `foldr` from Foldable.

-- sum

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs

-- product

product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs

-- elem

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = foldr (\a b -> b || a == x) False xs

-- minimum

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = foldr lt Nothing xs where
  lt a Nothing = Just a
  lt a (Just b) = if a < b then Just a else Just b

-- maximum

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = foldr gt Nothing xs where
  gt a Nothing = Just a
  gt a (Just b) = if a > b then Just a else Just b

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "sum'" $ do
        prop "is sum [1]" $
            ((\xs -> sum' xs == sum xs) :: ([Int] -> Bool))

        prop "is sum [2]" $
            ((\xs -> sum' xs == sum xs) :: (Maybe Int -> Bool))

    describe "product'" $ do
        prop "is product [1]" $
            ((\xs -> product' xs == product xs) :: ([Int] -> Bool))

        prop "is product [2]" $
            ((\xs -> product' xs == product xs) :: (Maybe Int -> Bool))

    describe "elem'" $ do
        prop "is elem [1]" $
            ((\x xs -> elem' x xs == elem x xs) :: (Int -> [Int] -> Bool))

        prop "is elem [2]" $
            ((\x xs -> elem' x xs == elem x xs) :: (Int -> Maybe Int -> Bool))

    describe "minimum'" $ do
        it "is like minimum" $ do
            minimum' [] `shouldBe` (Nothing :: Maybe Integer)
            minimum' [1] `shouldBe` (Just 1 :: Maybe Integer)
            minimum' [1, 42] `shouldBe` (Just 1 :: Maybe Integer)

    describe "maximum'" $ do
        it "is like maximum" $ do
            maximum' [] `shouldBe` (Nothing :: Maybe Integer)
            maximum' [1] `shouldBe` (Just 1 :: Maybe Integer)
            maximum' [1, 42] `shouldBe` (Just 42 :: Maybe Integer)
