module Main where

import Test.Hspec

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity f) (Identity a) = Identity (f a)

main :: IO ()
main = hspec $ do
    describe "Applicative instance for `Identity`" $ do
        it "pure" $ do
            pure (42 :: Integer) `shouldBe` Identity 42

        it "ap" $ do
            (Identity even) <*> (Identity (42 :: Integer)) `shouldBe` Identity True
            (Identity odd) <*> (Identity (42 :: Integer)) `shouldBe` Identity False
