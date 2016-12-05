isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
                     Nothing -> catMaybes xs
                     Just a -> a : catMaybes xs

-- From `Data.Maybe`:
catMaybes' xs = [x | Just x <- xs]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : xs) = Nothing
flipMaybe (Just x : xs) =
    case (flipMaybe xs) of
      Nothing -> Nothing
      Just xs' -> Just (x : xs')
