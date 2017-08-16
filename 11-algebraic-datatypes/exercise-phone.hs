import Data.Char (isUpper, toLower)


-- Valid buttons: one of "1234567890*#"
type Digit = Char

-- Valid number of key presses: [1..]
type Presses = Int


data PhoneKey = PhoneKey { chars :: String
                         , digit :: Digit}
                deriving (Eq, Show)

type DaPhone = [PhoneKey]

upperKey = PhoneKey { digit = '*', chars = ""}

daPhone = [ PhoneKey { digit = '1', chars = "" }
          , PhoneKey { digit = '2', chars = "abc" }
          , PhoneKey { digit = '3', chars = "def"}
          , PhoneKey { digit = '4', chars = "ghi"}
          , PhoneKey { digit = '5', chars = "jkl"}
          , PhoneKey { digit = '6', chars = "mno"}
          , PhoneKey { digit = '7', chars = "pqrs"}
          , PhoneKey { digit = '8', chars = "tuv"}
          , PhoneKey { digit = '9', chars = "wxyz"}
          , PhoneKey { digit = '0', chars = "+ "}
          , PhoneKey { digit = '#', chars = ".,"}
          ]

keyTaps :: Char -> PhoneKey -> Maybe [(Digit, Presses)]
keyTaps c k
    | c == digit k = Just [(digit k, length (chars k) + 1)]
    | c `elem` (chars k) = Just [(digit k, 1 + pos c (chars k) 0)]
    | isUpper(c) && (toLower c) `elem` (chars k) = Just [(digit upperKey, 1), (digit k, 1 + pos (toLower c) (chars k) 0)]
    | otherwise = Nothing
    where
      pos :: Char -> String -> Int -> Int
      pos _ [] _ = -1
      pos c (x:xs) n = if c == x then n
                       else pos c xs (n + 1)

tapsFor :: DaPhone -> Char -> [(Digit, Presses)]
tapsFor p c = go p c [] where
  go :: DaPhone -> Char -> [(Digit, Presses)] -> [(Digit, Presses)]
  go [] _ acc = acc
  go (k:ks) c acc = case (keyTaps c k) of
                      Just taps -> go ks c (acc ++ taps)
                      Nothing   -> go ks c acc
