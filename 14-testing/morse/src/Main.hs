module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

import Morse (morseToChar, stringToMorse)

convertFromMorse :: IO ()
convertFromMorse = forever $ do
    done <- hIsEOF stdin
    when done exitSuccess

    line <- hGetLine stdin
    convertLine line
      where
        convertLine line = do
            let decoded :: Maybe String
                decoded = traverse morseToChar (words line)
            case decoded of
              (Just s) -> putStrLn s
              Nothing -> do
                  putStrLn $ "ERROR: " ++ line
                  exitFailure

convertToMorse :: IO ()
convertToMorse = forever $ do
    done <- hIsEOF stdin
    when done exitSuccess

    line <- hGetLine stdin
    convertLine line
        where
          convertLine line = do
              let morse = stringToMorse line
              case morse of
                (Just s) -> putStrLn $ intercalate " " s
                Nothing -> do
                    putStrLn $ "ERROR: " ++ line
                    exitFailure

main :: IO ()
main = do
    mode <- getArgs
    case mode of
      [arg] ->
          case arg of
            "from" -> convertFromMorse
            "to" -> convertToMorse
            _ -> argError
      _ -> argError
  where
      argError = do
          putStrLn "Usage: morse <from|to>"
          exitFailure
