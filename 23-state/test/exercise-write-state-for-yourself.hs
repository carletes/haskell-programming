module Main where

import Control.Applicative
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Our `State` type.

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

-- Functor instance.

instance Functor (Moi s) where
    fmap f (Moi sa) = Moi (\s -> (f $ fst (sa s), s))

-- Applicative instance.

instance Applicative (Moi s) where
    pure a = Moi (\s -> (a, s))
    (Moi sab) <*> (Moi sa) = Moi (\s -> (fst (sab s) (fst (sa s)), s))

-- Test suite helpers.

instance (Monoid a) => Monoid (Moi s a) where
    mempty = Moi (\s -> (mempty, s))
    (Moi sa) `mappend` (Moi sb) = Moi (\s -> (fst (sa s) `mappend` fst (sb s), s))

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Moi s a) where
    arbitrary = fmap Moi arbitrary

instance Show (Moi s a) where
  show _ = "<Moi s a>"

instance (Arbitrary s, Show s, EqProp s, EqProp a) => EqProp (Moi s a) where
    (Moi sa) =-= (Moi sb) = property $ liftA2 (=-=) sa sb

-- Test suite driver.

main :: IO ()
main = hspec $
    describe "Moi s a" $ do
      testBatch $ functor (undefined :: (Moi String Int, (Int, Int, Int)))
      testBatch $ monoid (undefined :: Moi String String)
      testBatch $ applicative (undefined :: (Moi String String, (String, String, String)))
