module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip_pointFree :: (Show a, Read a) => a -> a
roundTrip_pointFree = read . show

roundTrip_changedType :: (Show a, Read b) => a -> b
roundTrip_changedType a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)
