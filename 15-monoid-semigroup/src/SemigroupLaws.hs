module SemigroupLaws
  ( semigroupAssoc
  )
  where

import Data.Semigroup

semigroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c
