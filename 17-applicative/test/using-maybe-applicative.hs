module Main where

-- A walk through applicative `Maybe`. The goal is to define a data type `Person`,
-- composed of a name and an addresss, with are both strings of give maximum
-- lengths.

import Test.Hspec

validateLength :: Int -> String -> Maybe String
validateLength n s =
    if length s <= n
    then Just s
    else Nothing

-- A `Name` is just a string of no more than 25 characters.

newtype Name = Name String deriving (Eq, Show)

-- Since `Maybe` is a functor, we map the data constructor over `Maybe`.

mkName :: String -> Maybe Name
mkName n = Name <$> validateLength 25 n

-- Similarly, an `Address` is just a string of up to 100 characters.

newtype Address = Address String deriving (Eq, Show)

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

-- Finally:

data Person = Person Name Address
    deriving (Eq, Show)

-- The applicative version of `mkName`.

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- The applicative `mkPerson` is simpler than the monadic one (although we do
-- not know yet what that is).

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = do
    n' <- mkName n
    a' <- mkAddress a
    return $ Person n' a'

main :: IO ()
main = hspec $ do
    describe "mkPerson" $ do
        it "Applicative" $ do
            mkPerson "Pepin de Triana" "42, His Street" `shouldBe`
                Just (Person (Name "Pepin de Triana") (Address "42, His Street"))

        it "Monadic" $ do
            mkPerson' "Pepin de Triana" "42, His Street" `shouldBe`
                Just (Person (Name "Pepin de Triana") (Address "42, His Street"))
