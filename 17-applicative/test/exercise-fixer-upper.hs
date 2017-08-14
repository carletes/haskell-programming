{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Test.Hspec

main :: IO ()
main = hspec $
    describe "Exercise: Fixer Upper" $ do
        it "const <$> Just \"Hello\" <*> \"World\"" $
            const <$> Just "Hello" <*> Just "World" `shouldBe` Just "Hello"

        it "(,,,) Just 90 <*> Just 10 Just \"Tierness\" [1, 2, 3]" $
            (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
                `shouldBe` Just (90, 10, "Tierness", [1, 2, 3])
