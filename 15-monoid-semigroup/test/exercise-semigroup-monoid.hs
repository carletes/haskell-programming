module Main where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)

import MonoidLaws
import SemigroupLaws

-- Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity

newtype Identity a = Identity a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

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

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

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

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

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

-- Combine a b

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)
    mappend = (<>)

type CombineAssoc =
    Combine Int (Sum Int) ->
    Combine Int (Sum Int) ->
    Combine Int (Sum Int) ->
    Bool

--Validation (AccumulateLeft)

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

newtype AccumulateLeft a b =
    AccumulateLeft (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (AccumulateLeft a b) where
    AccumulateLeft (Failure a) <> AccumulateLeft (Failure a') = AccumulateLeft (Failure (a <> a'))
    AccumulateLeft (Failure a) <> _ = AccumulateLeft (Failure a)
    AccumulateLeft _ <> AccumulateLeft (Failure a') = AccumulateLeft (Failure a')
    AccumulateLeft (Success b) <> AccumulateLeft (Success _) = AccumulateLeft (Success b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateLeft a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ AccumulateLeft (Failure a),
               return $ AccumulateLeft (Success b)]

type AccumulateLeftAssoc =
    AccumulateLeft String String ->
    AccumulateLeft String String ->
    AccumulateLeft String String ->
    Bool

-- Validation (AccumulateRight)

newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AccumulateRight a b) where
    AccumulateRight (Failure _) <> AccumulateRight (Failure a') = AccumulateRight (Failure a')
    AccumulateRight (Failure a) <> _ = AccumulateRight (Failure a)
    AccumulateRight _ <> AccumulateRight (Failure a') = AccumulateRight (Failure a')
    AccumulateRight (Success b) <> AccumulateRight (Success b') = AccumulateRight (Success (b <> b'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ AccumulateRight (Failure a),
               return $ AccumulateRight (Success b)]

type AccumulateRightAssoc =
    AccumulateRight String String ->
    AccumulateRight String String ->
    AccumulateRight String String ->
    Bool

-- Validation (AccumulateBoth)

newtype AccumulateBoth a b =
    AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Failure a) <> AccumulateBoth (Failure a') = AccumulateBoth (Failure (a <> a'))
    AccumulateBoth (Failure a) <> _ = AccumulateBoth (Failure a)
    AccumulateBoth _ <> AccumulateBoth (Failure a') = AccumulateBoth (Failure a')
    AccumulateBoth (Success b) <> AccumulateBoth (Success b') = AccumulateBoth (Success (b <> b'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ AccumulateBoth (Failure a),
               return $ AccumulateBoth (Success b)]

type AccumulateBothAssoc =
    AccumulateBoth String String ->
    AccumulateBoth String String ->
    AccumulateBoth String String ->
    Bool

-- Test suite

main :: IO ()
main = hspec $ do
    describe "Trivial" $ do
        it "is associative" $ do
            property (semigroupAssoc :: TrivialAssoc)

        it "has a left identity" $ do
            property (monoidLeftIdentity :: Trivial -> Bool)

        it "has a right identity" $ do
            property (monoidRightIdentity :: Trivial -> Bool)

    describe "Identity a" $ do
        it "is associative" $ do
            property (semigroupAssoc :: IdentityAssoc)

        it "has a left identity" $ do
            property (monoidLeftIdentity :: Identity String -> Bool)

        it "has a right identity" $ do
            property (monoidRightIdentity :: Identity String -> Bool)

    describe "Two a b" $ do
        it "is associative" $ do
            property (semigroupAssoc :: TwoAssoc)

        it "has a left identity" $ do
            property (monoidLeftIdentity :: Two String String -> Bool)

        it "has a right identity" $ do
            property (monoidRightIdentity :: Two String String -> Bool)

    describe "BoolConj" $ do
        it "is associative" $ do
            property (semigroupAssoc :: BoolConjAssoc)

        it "has a left identity" $ do
            property (monoidLeftIdentity :: BoolConj -> Bool)

        it "has a right identity" $ do
            property (monoidRightIdentity :: BoolConj -> Bool)

        describe "Required values" $ do
            it "True <> True = True" $ do
                (BoolConj True) <> (BoolConj True) `shouldBe` (BoolConj True)

            it "True <> False = False" $ do
                (BoolConj True) <> (BoolConj False) `shouldBe` (BoolConj False)

            it "True <> mempty = True" $ do
                (BoolConj True) <> mempty `shouldBe` (BoolConj True)

            it "mempty <> False = True" $ do
                mempty <> (BoolConj False) `shouldBe` (BoolConj False)

    describe "BoolDisj" $ do
        it "is associative" $ do
            property (semigroupAssoc :: BoolDisjAssoc)

        it "has a left identity" $ do
            property (monoidLeftIdentity :: BoolDisj -> Bool)

        it "has a right identity" $ do
            property (monoidRightIdentity :: BoolDisj -> Bool)

        describe "Required values" $ do
            it "True <> True = True" $ do
                (BoolDisj True) <> (BoolDisj True) `shouldBe` (BoolDisj True)

            it "True <> False = True" $ do
                (BoolDisj True) <> (BoolDisj False) `shouldBe` (BoolDisj True)

            it "True <> mempty = True" $ do
                (BoolDisj True) <> mempty `shouldBe` (BoolDisj True)

            it "mempty <> False = False" $ do
                mempty <> (BoolDisj False) `shouldBe` (BoolDisj False)

    describe "Or a b" $ do
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

    describe "Combine a b" $ do
        describe "Required values" $ do
            let f = Combine $ \n -> Sum (n + 1)
                g = Combine $ \n -> Sum (n - 1)

            it "(f <> g) $ 0 = 0" $ do
                (unCombine (f <> g) $ 0) `shouldBe` (Sum 0 :: Sum Int)

            it "(f <> g) $ 1 = 2" $ do
                (unCombine (f <> g) $ 1) `shouldBe` (Sum 2 :: Sum Int)

            it "(f <> f) $ 1 = 4" $ do
                (unCombine (f <> f) $ 1) `shouldBe` (Sum 4 :: Sum Int)

            it "(g <> f) $ 1 = 2" $ do
                (unCombine (g <> f) $ 1) `shouldBe` (Sum 2 :: Sum Int)

            it "(f <> mempty) $ 1 = 2" $ do
                (unCombine (f <> mempty) $ 1) `shouldBe` (Sum 2 :: Sum Int)

    describe "AccumulateLeft a b" $ do
        it "is associative" $ do
            property (semigroupAssoc :: AccumulateLeftAssoc)

    describe "AccumulateRight a b" $ do
        it "is associative" $ do
            property (semigroupAssoc :: AccumulateRightAssoc)

    describe "AccumulateBoth a b" $ do
        it "is associative" $ do
            property (semigroupAssoc :: AccumulateBothAssoc)
