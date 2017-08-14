module Main where

import Test.Hspec

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

main :: IO ()
main = hspec $
    describe "Applicative instance for `Identity`" $ do
        it "pure" $
            pure (42 :: Integer) `shouldBe` Identity 42

        it "ap" $ do
            Identity even <*> Identity (42 :: Integer) `shouldBe` Identity True
            Identity odd <*> Identity (42 :: Integer) `shouldBe` Identity False
