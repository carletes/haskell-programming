module Kleisli where

import Control.Monad

-- Monad composition

-- First version.
--
-- The straightforward `f . g` does not work, because `g a` gives `m b`,
-- and `f` takes `b`. So we `fmap`, in order to make `f` work inside the
-- structure imposed by `m`:
--
--     f <$> (g a)
--
-- But `f` gives `m c`, so what we get is `m (m c)`. We need `join` to remove
-- the extra `m`.

mcomp :: Monad m =>
         (b -> m c) ->
         (a -> m b) ->
         a -> m c
mcomp f g a = join $ f <$> (g a)

-- Second version.
--
-- We saw at the beginning of the chapter that `>>=` can be expressed in terms
-- of `fmap` and `join`, which is what we are doing in `mcomp`.

mcomp' :: Monad m =>
          (b -> m c) ->
          (a -> m b) ->
          a -> m c
mcomp' f g a = g a >>= f

-- Our `mcomp` is veery similar to the "Kleisli fish" (`>=>`) from
-- `Control.Monad`:
--
--     λ :t (>=>)
--     (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--     λ
--
-- An example of monad composition:

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"

-- The "reversed Kleisli fish" (`<=<`) expresses monadic composition in the same
-- order as the usual `.`:

getAge' :: String -> IO Int
getAge' = readM <=< sayHi

askForAge' :: IO Int
askForAge' = getAge' "Hello! How old are you?"
