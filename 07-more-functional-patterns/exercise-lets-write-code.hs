tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = d `mod` 10
    where (d, _) = x `divMod` 10

hunsD x = (x `div` 100) `mod` 10

foldBool_case :: a -> a -> Bool -> a
foldBool_case x y p = case p of
  True  -> x
  False -> y

foldBool_guard :: a -> a -> Bool -> a
foldBool_guard x y p
    | p == True = x
foldBool_guard x y p
    | p == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
