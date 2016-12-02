import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world"
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f [] where
  f (DbDate d) lst = [d] ++ lst
  f _ lst = lst

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] where
  f (DbNumber n) lst = [n] ++ lst
  f _ lst = lst

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sum dbNumbers)) / (fromIntegral (length dbNumbers)) where
  dbNumbers = filterDbNumber db
