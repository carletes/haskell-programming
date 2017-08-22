{-# LANGUAGE InstanceSigs #-}

-- We use `InstanceSigs` above, so that we may write the signatures of the
-- functions for `Functor`, `Applicative` and `Monad`, making it easier for us
-- to implement.

module Main where

import System.Exit

-- The definition of `Reader`.
newtype Reader r a =
    Reader { runReader :: r -> a }

-- `Functor` for `Reader` (required by `Applicative`)
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader (f . ra)

-- `Applicative` for `Reader` (required by `Monad`).
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader (const a)  -- The only thing we can do!

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader rab <*> Reader ra = Reader (\r -> rab r (ra r))

-- The aim of this exercise: `Monad` for `Reader`.
instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    -- We just follow the types (and the hints from the text ;))
    Reader ra >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r


main :: IO ()
main = exitSuccess
