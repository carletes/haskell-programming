module Main where

import Data.Char
import Test.Hspec
import Test.Hspec.QuickCheck

cap :: String -> String
cap = fmap toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupledMonadic :: String -> (String, String)
tupledMonadic = do
    a <- cap
    b <- rev
    return (a, b)

main :: IO ()
main = hspec $ do
    describe "composed" $
        it "behaves as expected" $ do
            composed "Julie" `shouldBe` "EILUJ"
            composed "Chris" `shouldBe` "SIRHC"

    describe "fmapped" $
        it "behaves as expected" $ do
            fmapped "Julie" `shouldBe` "EILUJ"
            fmapped "Chris" `shouldBe` "SIRHC"

    describe "composed and fmapped " $
        prop "are the same" $
            \xs -> composed xs == fmapped xs

    describe "tupled" $
        it "behaves as expected" $
            tupled "Julie" `shouldBe` ("JULIE", "eiluJ")

    describe "tupledMonadic" $
        prop "is like `tupled`" $
            \xs -> tupled xs == tupledMonadic xs
