myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x:xs) = if p x then True else myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if x == y then True else myElem y xs

myElem' x xs = any (== x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ (squish ls)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f (x:xs) = go f xs x where
  go _ [] acc = acc
  go f (x:xs) acc = if f x acc == GT then go f xs x else go f xs acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f (x:xs) = go f xs x where
  go _ [] acc = acc
  go f (x:xs) acc = if f x acc == LT then go f xs x else go f xs acc
