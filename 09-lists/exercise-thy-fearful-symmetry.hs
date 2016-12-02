myWords :: String -> [String]
myWords s = go s [] where
  go "" acc = reverse acc
  go s acc = go xs (x : acc) where
    x = takeWhile (/= ' ') s
    xs = dropWhile (== ' ') $ dropWhile (/= ' ') s
