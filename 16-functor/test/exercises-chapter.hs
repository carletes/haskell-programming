module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

-- `Bool` has kind `*`, so no functor instance possible.
--
-- BoolAndSomethingelse a

data BoolAndSomethingElse a =
    False' a | True' a
    deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
    arbitrary = do
        a <- arbitrary
        oneof [return (False' a),
               return (True' a)]

boolAndSomethingElseIdentity :: BoolAndSomethingElse Int -> Bool
boolAndSomethingElseIdentity = functorIdentity

boolAndSomethingElseComposition :: BoolAndSomethingElse Int -> Fun Int Int -> Fun Int Int -> Bool
boolAndSomethingElseComposition = functorComposition

-- BoolAndMaybeSomethingelse a

data BoolAndMaybeSomethingElse a =
    Falsish | Trueish a
    deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Trueish a) = Trueish (f a)

instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary = do
        a <- arbitrary
        oneof [return Falsish,
               return (Trueish a)]

boolAndMaybeSomethingElseIdentity :: BoolAndSomethingElse Int -> Bool
boolAndMaybeSomethingElseIdentity = functorIdentity

boolAndMaybeSomethingElseComposition :: BoolAndSomethingElse Int -> Fun Int Int -> Fun Int Int -> Bool
boolAndMaybeSomethingElseComposition = functorComposition

--- The kind of `newtype Mu f = InF { outF :: f (Mu f) }` is `(* -> *) -> *.
--- I do not know how to define a functor instance for it.

--- The kind of `data D = D (Array Word Word) Int Int` is `*`, so no functor
--- instance is possible.

--- Sum a b => Sum b a

data Sum b a =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum b a) where
    arbitrary = do
        b <- arbitrary
        a <- arbitrary
        oneof [return $ First a,
               return $ Second b]

sumIdentity :: Sum Int Int -> Bool
sumIdentity = functorIdentity

sumComposition :: Sum Int Int -> Fun Int Int -> Fun Int Int -> Bool
sumComposition = functorComposition

-- Company a b c => Company a c b

data Company a c b =
    DeepBlue a c
    | Something b
    deriving (Eq, Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a c b) where
    arbitrary = do
        a <- arbitrary
        c <- arbitrary
        b <- arbitrary
        oneof [return $ DeepBlue a c,
               return $ Something b]

companyIdentity :: Company Int Int Int -> Bool
companyIdentity = functorIdentity

companyComposition :: Company Int Int Int -> Fun Int Int -> Fun Int Int -> Bool
companyComposition = functorComposition

--- More a b => More b a

data More b a =
    L a b a
    | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ L a b a,
               return $ R b a b]

moreIdentity :: More Int Int -> Bool
moreIdentity = functorIdentity

moreComposition :: More Int Int-> Fun Int Int -> Fun Int Int -> Bool
moreComposition = functorComposition

--- Quant a b

data Quant a b =
    Finance
  | Desk a
  | Floor b
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Floor b) = Floor (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return Finance,
               return $ Desk a,
               return $ Floor b]

quantIdentity :: Quant Int Int -> Bool
quantIdentity = functorIdentity

quantComposition :: Quant Int Int-> Fun Int Int -> Fun Int Int -> Bool
quantComposition = functorComposition


--- Test suite driver

main :: IO ()
main = hspec $ do
    describe "The functor for BoolAndSomethingElse a" $ do
        it "follows the identity law" $ do
            property boolAndSomethingElseIdentity

        it "follows the composition law" $ do
            property boolAndSomethingElseComposition

    describe "The functor for BoolAndMaybeSomethingElse a" $ do
        it "follows the identity law" $ do
            property boolAndMaybeSomethingElseIdentity

        it "follows the composition law" $ do
            property boolAndMaybeSomethingElseComposition

    describe "The functor for Sum b a" $ do
        it "follows the identity law" $ do
            property sumIdentity

        it "follows the composition law" $ do
            property sumComposition

    describe "The functor for Company a c b" $ do
        it "follows the identity law" $ do
            property companyIdentity

        it "follows the composition law" $ do
            property companyComposition

    describe "The functor for More b a" $ do
        it "follows the identity law" $ do
            property moreIdentity

        it "follows the composition law" $ do
            property moreComposition

        it "works as exepected (L)" $ do
            (fmap (+1) (L 1 2 3) :: More Int Int) `shouldBe` L 2 2 4

        it "works as exepected (R)" $ do
            (fmap (+1) (R 1 2 3) :: More Int Int) `shouldBe` R 1 3 3

    describe "The functor for Quant a b" $ do
        it "follows the identity law" $ do
            property quantIdentity

        it "follows the composition law" $ do
            property quantComposition
