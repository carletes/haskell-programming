import Data.List (intersperse)

notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe = concat . (intersperse " ") . (map replace . words) where
  replace :: String -> String
  replace s =
      case (notThe s) of
        Nothing -> "a"
        Just w -> w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0 where
  go :: [String] -> Integer -> Integer
  go [] acc = acc
  go (w:ws) acc = if w == "the" then go ws (acc + startsWithVowel ws)
                  else go ws acc
  startsWithVowel :: [String] -> Integer
  startsWithVowel [] = 0
  startsWithVowel (w:_) = if head w `elem` "aeiou" then 1 else 0

countVowels :: String -> Int
countVowels = length . filter (`elem` "aeiou")
