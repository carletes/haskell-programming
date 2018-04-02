module Main where

import System.Random   (StdGen, mkStdGen, randomR)
import Test.Hspec
import Test.QuickCheck

import RandomExample   (Die, intToDie)
import RandomExample2  (rollsToGetTwenty)

instance Arbitrary StdGen where
    arbitrary = do
        seed <- arbitrary
        return $ mkStdGen seed

-- Refactor `rollsToGetTwenty` into having the limit be a function argument.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go n 0 0
    where
      go :: Int -> Int -> Int -> StdGen -> Int
      go limit acc count gen
          | acc >= limit = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go limit (acc + die) (count + 1) nextGen

-- Change `rollsToGetN` to recording the series of die that occurred in addition to the count.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go n (0, []) 0
    where
      go :: Int -> (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
      go limit (acc, throws) count gen
          | acc >= limit = (count, reverse throws)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go limit (acc + die, intToDie die : throws) (count + 1) nextGen

prop_rollsCountLogged_returnVal :: Int -> StdGen -> Bool
prop_rollsCountLogged_returnVal n g =
    let (count, throws) = rollsCountLogged n g
    in
      length throws == count


-- Test suite driver.

main :: IO ()
main = hspec $ do
    describe "rollsToGetN" $ do
        it "extends `rollsToGetTwenty`" $
            property $ \g -> rollsToGetN 20 g == rollsToGetTwenty g

    describe "rollsCountlogged" $ do
        it "returns lists of the right length" $
            property prop_rollsCountLogged_returnVal
