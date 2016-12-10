module Main where

import qualified Data.Map as M
import Test.QuickCheck (Gen, Property, elements, forAll, quickCheck)

import Morse (Morse, charToMorse, letterToMorse, morseToChar)

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

charMorse :: Gen Morse
charMorse = elements allowedMorse

prop_roundtrip :: Property
prop_roundtrip =
    forAll charGen
    (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_roundtrip
