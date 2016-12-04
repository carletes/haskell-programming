myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)

myElem' a = foldr (\x b -> x == a || b) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x b -> (f x) : b) []

squish :: [[a]] -> [a]
squish = foldr (\x b -> x ++ b) []
