data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0 = Nothing
               | n == 0 = Just Zero
               | otherwise = Just $ Succ n' where
                   Just n' = integerToNat (n - 1)
