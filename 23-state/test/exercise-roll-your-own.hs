module Main where

import System.Random (StdGen, randomR)

-- Refactor rollsToGetTwenty into having the limit be a function argument.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go n 0 0
    where
      go :: Int -> Int -> Int -> StdGen -> Int
      go limit acc count gen
          | acc >= limit = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go limit (acc + die) (count + 1) nextGen

main :: IO ()
main = undefined
