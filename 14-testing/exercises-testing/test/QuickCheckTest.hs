module Main where

import Test.Hspec
import Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

main :: IO ()
main = hspec $ do
    describe "half" $ do
        it "is the inverse of `(*2)`" $ do
            property $ \x -> 2 * (half x) == (x :: Double)

    describe "integer addition" $ do
        it "is commutative" $ do
            property $ \x y -> x + y == ((y + x) :: Integer)

        it "is associative" $ do
            property $ \x y z -> x + (y + z) == (((x + y) + z) :: Integer)

    describe "integer multiplication" $ do
        it "is commutative" $ do
            property $ \x y -> x * y == ((y * x) :: Integer)

        it "is associative" $ do
            property $ \x y z -> x * (y * z) == (((x * y) * z) :: Integer)

        it "is distributive for addition" $ do
            property $ \x y z -> x * (y + z) == (((x * y) + (x * z)) :: Integer)

    describe "lists" $ do
        it "reversing twice is the identity" $ do
            property $ \s -> reverse (reverse s) == (s :: String)
