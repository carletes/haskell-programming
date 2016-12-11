module Main where

import Data.Monoid
import Test.QuickCheck
import Test.Hspec

import Optional

main :: IO ()
main = hspec $ do
    describe "Expected output" $ do
        it "works for Sum" $ do
            (Only (Sum 1) <> Only (Sum 1)) `shouldBe` Only (Sum (2 :: Int))
        it "works for Product" $ do
            (Only (Product 4) <> Only (Product 2)) `shouldBe` Only (Product (8 :: Int))
        it "works for []" $ do
            (Only [42] <> Nada) `shouldBe` (Only [42 :: Int])

    describe "Monoid laws" $ do
        it "is associative" $ do
            property $ \a b c -> a <> (b <> c) == (a <> b) <> (c :: (Optional [Int]))
        it "has left identity" $ do
            property $ \a -> (Nada <> a) == (a :: Optional [Int])
        it "has right identity" $ do
            property $ \a -> (a <> Nada) == (a :: Optional [Int])
