module Main where

import Data.Semigroup
import Test.Hspec
import Test.QuickCheck

import SemigroupLaws

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity

newtype Identity a = Identity a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- Two a b

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoAssoc = Two String String -> Two String String  -> Two String String -> Bool

-- BoolConj

newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a && b)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Or a b

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst _) <> (Snd b) = Snd b
    (Fst _) <> (Fst a) = Fst a
    (Snd b) <> (Fst _) = Snd b
    (Snd b) <> (Snd _) = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a,
               return $ Snd b]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- Test suite

main :: IO ()
main = hspec $ do
    describe "Trivial" $ do
        it "is associative" $ do
            property (semigroupAssoc :: TrivialAssoc)

    describe "Identity a" $ do
        it "is associative" $ do
            property (semigroupAssoc :: IdentityAssoc)

    describe "Two a b" $ do
        it "is associative" $ do
            property (semigroupAssoc :: TwoAssoc)

    describe "BoolConj" $ do
        it "is associative" $ do
            property (semigroupAssoc :: BoolConjAssoc)

    describe "BoolDisj" $ do
        it "is associative" $ do
            property (semigroupAssoc :: BoolDisjAssoc)

    describe "Or" $ do
        it "is associative" $ do
            property (semigroupAssoc :: OrAssoc)

        describe "Required values" $ do
            it "Fst <> Snd" $ do
                (Fst 1 <> Snd 2) `shouldBe` (Snd 2 :: Or Int Int)

            it "Fst <> Fst" $ do
                (Fst 1 <> Fst 2) `shouldBe` (Fst 2 :: Or Int Int)

            it "Snd <> Fst" $ do
                (Snd 1 <> Fst 2) `shouldBe` (Snd 1 :: Or Int Int)

            it "Snd <> Snd" $ do
                (Snd 1 <> Snd 2) `shouldBe` (Snd 1 :: Or Int Int)
