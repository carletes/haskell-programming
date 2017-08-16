addOneIfOdd n = case odd n of
  True  -> f n
  False ->  n
  where f = \n -> n + 1

-- Interesting: Without type annotations, the inferred type for `addFive` is:
--
--     (Ord a, Num a) => a -> a -> a
--
-- but the inferred type of `addFive'` is:
--
--     Integer -> Integer -> Integer
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

-- Removing anonymous lambda syntax
mflip f = \x -> \y -> f y x

mflip' f x y = f y x
