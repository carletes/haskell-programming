module Main where

import Data.Foldable
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Poly

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
  lt a Nothing  = Just a
  lt a (Just b) = if a < b then Just a else Just b

-- maximum

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = foldr gt Nothing xs where
  gt a Nothing  = Just a
  gt a (Just b) = if a > b then Just a else Just b

-- null

null' :: Foldable t => t a -> Bool
null' xs = foldr (\_ _ -> False) True xs

-- length

length' :: Foldable t => t a -> Int
length' xs = foldr (\_ b -> b + 1) 0 xs

-- toList

toList' :: Foldable t => t a -> [a]
toList' xs = foldr (\a b -> a : b) [] xs

-- fold (using `foldMap`)

fold' :: (Foldable t, Monoid m) => t m -> m
fold' xs = foldMap id xs

-- foldMap (using `foldr`)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\a b -> (f a) <> b) mempty xs

-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "sum'" $ do
        prop "is sum [1]" $
            ((\xs -> sum' xs == sum xs) :: [Int] -> Bool)

        prop "is sum [2]" $
            ((\xs -> sum' xs == sum xs) :: Maybe Int -> Bool)

    describe "product'" $ do
        prop "is product [1]" $
            ((\xs -> product' xs == product xs) :: [Int] -> Bool)

        prop "is product [2]" $
            ((\xs -> product' xs == product xs) :: Maybe Int -> Bool)

    describe "elem'" $ do
        prop "is elem [1]" $
            ((\x xs -> elem' x xs == elem x xs) :: A -> [A] -> Bool)

        prop "is elem [2]" $
            ((\x xs -> elem' x xs == elem x xs) :: A -> Maybe A -> Bool)

    describe "minimum'" $ do
        it "is like minimum" $ do
            minimum' [] `shouldBe` (Nothing :: Maybe Int)
            minimum' [1] `shouldBe` (Just 1 :: Maybe Int)
            minimum' [1, 42] `shouldBe` (Just 1 :: Maybe Int)

    describe "maximum'" $ do
        it "is like maximum" $ do
            maximum' [] `shouldBe` (Nothing :: Maybe Int)
            maximum' [1] `shouldBe` (Just 1 :: Maybe Int)
            maximum' [1, 42] `shouldBe` (Just 42 :: Maybe Int)

    describe "null'" $ do
        prop "is null [1]" $
            ((\xs -> null' xs == null xs) :: [A] -> Bool)

        prop "is null [2]" $
            ((\xs -> null' xs == null xs) :: Maybe A -> Bool)

    describe "length'" $ do
        prop "is length [1]" $
            ((\xs -> length' xs == length xs) :: [A] -> Bool)

        prop "is length [2]" $
            ((\xs -> length' xs == length xs) :: Maybe A -> Bool)

    describe "toList'" $ do
        prop "is toList [1]" $
            ((\xs -> toList' xs == toList xs) :: [A] -> Bool)

        prop "is toList [2]" $
            ((\xs -> toList' xs == toList xs) :: Maybe A -> Bool)

    describe "fold'" $ do
        prop "is fold [1]" $
            ((\xs -> fold' xs == fold xs) :: [Sum Int] -> Bool)

        prop "is fold [2]" $
            ((\xs -> fold' xs == fold xs) :: Maybe (Sum Int) -> Bool)

    describe "foldMap'" $ do
        prop "is foldMap [1]" $
            ((\xs -> foldMap' id xs == foldMap id xs) :: [Sum Int] -> Bool)

        prop "is foldMap [2]" $
            ((\xs -> foldMap' id xs == foldMap id xs) :: Maybe (Sum Int) -> Bool)
