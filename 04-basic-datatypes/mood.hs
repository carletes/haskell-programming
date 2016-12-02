-- mood.hs

-- Using the same identifier for the module name and the data type seems to work
-- so far (meaning that the module gets properly loaded into GHCi, and
-- `changeMood` is available).

module Mood where

data Mood = Blah | Woot
          deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
