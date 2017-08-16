module Main where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

import MonoidLaws
import Optional

newtype First' a =
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

instance Monoid (First' a) where
    mappend (First' (Only a)) _             = (First' (Only a))
    mappend (First' Nada) (First' (Only b)) = (First' (Only b))
    mappend (First' Nada) (First' Nada)     = First' Nada

    mempty = First' Nada

instance (Arbitrary a) => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ First' (Only a),
               return $ First' Nada]

type FirstAssoc = First' String -> First' String -> First' String -> Bool
type FirstId = First' String -> Bool

main :: IO ()
main = hspec $ do
    describe "Monoid laws" $ do
        it "is associative" $ do
            property (monoidAssoc :: FirstAssoc)
        it "has left identity" $ do
            property (monoidLeftIdentity :: FirstId)
        it "has right identity" $ do
            property (monoidRightIdentity :: FirstId)

    describe "Expected results" $ do
        it "Only a <> Nada = Only a" $ do
            (First' (Only 1) <> First' Nada ) `shouldBe` (First' (Only (1 :: Int)))
        it "Nada <> Nada = Nada" $ do
            (First' Nada <> First' Nada ) `shouldBe` ((First' Nada) :: (First' Int))
        it "Nada <> Only b = Only b" $ do
            (First' Nada <> First' (Only 2) ) `shouldBe` First' (Only (2 :: Int))
        it "Only a <> Only b = Only a" $ do
            (First' (Only 1) <> First' (Only 2) ) `shouldBe` First' (Only (1 :: Int))
