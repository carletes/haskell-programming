{-# LANGUAGE RankNTypes #-}

module Main where

import System.Exit

--- Natural transformations are mappings that transform the _structure_, but
--- preserver the types of the _content_.
---
--- For instance, given any functors `f` and `g`:

type Nat f g = forall a. f a -> g a

--- An example of a natural transformation:

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]

main :: IO ()
main = exitSuccess
