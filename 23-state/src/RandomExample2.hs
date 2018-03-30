module RandomExample2 where

import Control.Applicative       (liftA3)
import Control.Monad             (replicateM)
import Control.Monad.Trans.State
import System.Random

import RandomExample

-- `rollDie`: Initial approach.
rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

-- `rollDie`: Less verbose
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
    liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
    where
      go :: Int -> Int -> StdGen -> Int
      go acc count gen
          | acc >= 20 = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (acc + die) (count + 1) nextGen
