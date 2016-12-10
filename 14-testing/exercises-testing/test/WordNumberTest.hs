module Main where

import Test.Hspec

import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "returns `zero` for 0" $ do
            digitToWord 0 `shouldBe` "zero"

    describe "digits" $ do
        it "returns `[1]` for 1" $ do
            digits 1 `shouldBe` [1]

    describe "wordNumber" $ do
        it "returns `four-two` for 42" $ do
            wordNumber 42 `shouldBe` "four-two"
