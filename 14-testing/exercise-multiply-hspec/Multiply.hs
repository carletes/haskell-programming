module Multiply where

import Test.Hspec

multiply :: (Eq a, Num a) => a -> a -> a
multiply m n = go m n 0
  where
    go m n acc
        | n == 0 = acc
        | otherwise = go m (n - 1) (acc + m)

main :: IO ()
main = hspec $ do
    describe "Multiply" $ do
        it "2 * 3 = 6" $ do
            (2 `multiply` 3) `shouldBe` 6
