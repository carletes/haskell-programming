module Main where

import System.Exit

-- The definition of `Reader`.
newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

main :: IO ()
main = exitSuccess
