newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = let vs = filter (`elem` vowels) s
               cs = filter (`notElem` vowels) s
           in
             if length vs > length cs then Nothing else Just $ Word' s
