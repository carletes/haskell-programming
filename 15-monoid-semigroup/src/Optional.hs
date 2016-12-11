module Optional where

import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mappend (Only a) (Only b) = Only (mappend a b)
    mappend (Only a) Nada = Only a
    mappend Nada (Only b) = Only b
    mappend Nada Nada = Nada

    mempty = Nada

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ Only a,
               return Nada]
