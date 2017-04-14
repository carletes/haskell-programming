module Main where

import Control.Monad (join)
import System.Exit

-- Write `bind` in terms of `join`.

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

main :: IO ()
main = exitSuccess
