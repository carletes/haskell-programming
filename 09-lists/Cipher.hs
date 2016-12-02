module Cipher
  ( caesar
  , uncaesar
  ) where

import Data.Char (chr, ord)


caesar :: Int -> String -> String
caesar _ "" = ""
caesar k (x:xs) = encodeChar k x : caesar k xs where
  encodeChar :: Int -> Char -> Char
  encodeChar k c
      | c' >= ord_a && c' <= ord_z = chr $ ord_a + (c' - ord_a + k) `mod` 26
      | c' >= ord_A && c' <= ord_Z = chr $ ord_A + (c' - ord_A + k) `mod` 26
      | otherwise = c where
          c' = ord c
          ord_a = ord 'a'
          ord_z = ord 'z'
          ord_A = ord 'A'
          ord_Z = ord 'Z'

uncaesar k = caesar (-k)
