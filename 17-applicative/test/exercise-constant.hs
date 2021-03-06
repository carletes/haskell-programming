module Main where

import Test.Hspec

newtype Constant a b = Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant _) (Constant a) = Constant a

main :: IO ()
main = hspec $
    describe "Applicative instance for `Constant`" $ do
        it "pure" $
            pure (42 :: Integer) `shouldBe` Constant ""
        it "ap" $
            pure (even :: Integer -> Bool) <*> Constant "" `shouldBe` Constant ""
