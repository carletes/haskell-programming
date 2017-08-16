lefts' :: [Either a b] -> [a]
lefts' xs = foldr f [] xs where
  f :: Either a b -> [a] -> [a]
  f (Left a) as = a : as
  f _ as        = as

rights' :: [Either a b] -> [b]
rights' xs = foldr f [] xs where
  f :: Either a b -> [b] -> [b]
  f (Right b) bs = b : bs
  f _ bs         = bs

-- From `Data.Either`:
lefts xs = [x | Left x <- xs]
rights xs = [x | Right x <- xs]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr f ([], []) xs where
  f :: Either a b -> ([a], [b]) -> ([a], [b])
  f (Left a) (as, bs)  = (a : as, bs)
  f (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b
