module Beeps where

import Control.Applicative

-- As mentioned in page 834, the following function signature is required,
-- should we wish to make `boop` polymorphic:
--
--     boop :: Num a => a -> a
boop :: Integer -> Integer
boop = (*2)

-- As mentioned in page 834, the following function signature is required,
-- should we wish to make `doop` polymorphic:
--
--     doop :: Num a => a -> a
doop :: Integer -> Integer
doop = (+10)

-- As mentioned in page 834, the following function signature is required,
-- should we wish to make `bip` polymorphic:
--
--     bip :: Num a => a -> a
bip :: Integer -> Integer
bip = boop . doop

-- Functions as functors:
bloop :: Integer -> Integer
bloop = fmap boop doop

-- Functions as applicatives:
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


-- Functions as mondas
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)
