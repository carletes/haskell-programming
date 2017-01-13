module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Possibly a

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
    fmap _ (LolNope) = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

instance (Arbitrary a) => Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        oneof [return LolNope,
               return $ Yeppers a]

possiblyIdentity :: Possibly Int -> Bool
possiblyIdentity = functorIdentity

possiblyComposition :: Possibly Int -> Fun Int Int -> Fun Int Int -> Bool
possiblyComposition = functorComposition

--- Let's go!

main :: IO ()
main = hspec $ do
    describe "Possibly a" $ do
        it "follows the identity law" $ do
            property possiblyIdentity

        it "follows the composition law" $ do
            property possiblyComposition
