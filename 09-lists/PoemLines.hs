module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = go s [] where
  go :: String -> [String] -> [String]
  go "" acc = reverse acc
  go s acc = go (xs) (x : acc) where
    x = takeWhile (/= '\n') s
    xs = dropWhile (== '\n') $ dropWhile (/= '\n') s

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]
