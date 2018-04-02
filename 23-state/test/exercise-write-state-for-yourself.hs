module Main where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function


-- Our `State` type.

newtype Moi s a =
    Moi { runMoi :: s -> (a, s)}

-- Functor instance.

instance Functor (Moi s) where
    fmap f (Moi sa) = Moi (\s -> (f $ fst (sa s), s))

-- Test suite helpers.

-- Test suite driver.

main :: IO ()
main = hspec $
    describe "Moi s a" $ do
      testBatch $ functor (undefined :: (Moi String Int, (Int, Int, Int)))
