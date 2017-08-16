zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

zipFromZipWith = zipWith' (\x y -> (x, y))
