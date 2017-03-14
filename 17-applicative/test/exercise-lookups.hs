{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Data.List (elemIndex)
import Test.Hspec

-- Make the following expressions type-check using `pure`, `<$>` and `<*>`

-- added :: Maybe Integer
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- y :: Maybe Integer
-- z :: Maybe Integer
--
-- tupled :: Maybe (Integer, Integer)
-- tupled = (,) y z

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- x :: Maybe Int
-- y' :: Maybe Int
--
-- maxed :: Maybe Int
-- maxed = max x y'

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

maxed :: Maybe Int
maxed = max <$> x <*> y'

-- xs :: [Integer]
-- ys :: [Integer]
-- xi :: Maybe Integer
-- yi :: Mabe Integer
--
-- summed :: Maybe Integer
-- summed = sum $ (,) xi yi

xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

xi :: Maybe Integer
xi = lookup 3 $ zip xs ys

yi :: Maybe Integer
yi = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> xi <*> yi)

main :: IO ()
main = hspec $ do
    describe "Expressions type-check" $ do
        it "added" $ do
            added `shouldBe` (Just 9)

        it "tupled" $ do
            tupled `shouldBe` Just (6, 5)

        it "maxed" $ do
            maxed `shouldBe` Just 3

        it "summed" $ do
            summed `shouldBe` Just 5
