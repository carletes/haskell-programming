-- An introduction to `checkers`, a library for verifying laws for several
-- standard Haskell type classes (like `Monoid` and `Applicative`).

module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Our target datat type.

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

-- Our monoid definition for `Bull`.

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

-- Let QuickCheck generate instances of `Bool`

instance Arbitrary Bull where
    arbitrary =
        frequency [(1, return Fools),
                   (1, return Twoo)]

-- Let `checkers` compare instances of `Bool`.

instance EqProp Bull where
    (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
