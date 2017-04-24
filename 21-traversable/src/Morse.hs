module Morse
  ( Morse
  , morseToChar
  , morseToLetter
  , letterToMorse
  , stringToMorse
  ) where

import qualified Data.Map as M

-- This is the new `stringToMorse`, much simpler than the original one:
--
--     stringToMorse s = sequence $ fmap charToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

-- The rest is pretty much like what we wrote in Chapter 14.

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [
      ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter =
    M.foldWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c =
    M.lookup c letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m =
    M.lookup m morseToLetter
