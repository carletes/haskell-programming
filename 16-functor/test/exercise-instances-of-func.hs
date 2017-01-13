module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import FunctorLaws

--- Identity a

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

identityIdentity :: Identity Int -> Bool
identityIdentity = functorIdentity

identityComposition :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool
identityComposition = functorComposition

-- Pair a a

data Pair a = Pair a a
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a

pairIdentity :: Pair Int -> Bool
pairIdentity = functorIdentity

pairComposition :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool
pairComposition = functorComposition

-- Two a b

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

twoIdentity :: Two Int Int -> Bool
twoIdentity = functorIdentity

twoComposition :: Two Int Int -> Fun Int Int -> Fun Int Int -> Bool
twoComposition = functorComposition

-- Three a b c

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

threeIdentity :: Three Int Int Int -> Bool
threeIdentity = functorIdentity

threeComposition :: Three Int Int Int -> Fun Int Int -> Fun Int Int -> Bool
threeComposition = functorComposition

-- Three' a b

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

threePrimeIdentity :: Three' Int Int -> Bool
threePrimeIdentity = functorIdentity

threePrimeComposition :: Three' Int Int -> Fun Int Int -> Fun Int Int -> Bool
threePrimeComposition = functorComposition

-- Four a b c d

data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

fourIdentity :: Four Int Int Int Int -> Bool
fourIdentity = functorIdentity

fourComposition :: Four Int Int Int Int -> Fun Int Int -> Fun Int Int -> Bool
fourComposition = functorComposition

--- Four' a b

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four' a b c d

fourPrimeIdentity :: Four' Int Int -> Bool
fourPrimeIdentity = functorIdentity

fourPrimeComposition :: Four' Int Int -> Fun Int Int -> Fun Int Int -> Bool
fourPrimeComposition = functorComposition

--- Trivial

--- No instance of Functor can be defined for data type `Trivial`, since:
---    λ data Trivial = Trivial
---    λ :k Trivial
---    Trivial :: *
---    λ

--- Let's go!

main :: IO ()
main = hspec $ do
    describe "Identity a" $ do
        it "follows the identity law" $ do
            property identityIdentity

        it "follows the composition law" $ do
            property identityComposition

    describe "Pair a" $ do
        it "follows the identity law" $ do
            property pairIdentity

        it "follows the composition law" $ do
            property pairComposition

    describe "Two a b" $ do
        it "follows the identity law" $ do
            property twoIdentity

        it "follows the composition law" $ do
            property twoComposition

    describe "Three a b c" $ do
        it "follows the identity law" $ do
            property threeIdentity

        it "follows the composition law" $ do
            property threeComposition

    describe "Three' a b" $ do
        it "follows the identity law" $ do
            property threePrimeIdentity

        it "follows the composition law" $ do
            property threePrimeComposition

    describe "Four a b c d" $ do
        it "follows the identity law" $ do
            property fourIdentity

        it "follows the composition law" $ do
            property fourComposition

    describe "Four' a b" $ do
        it "follows the identity law" $ do
            property fourPrimeIdentity

        it "follows the composition law" $ do
            property fourPrimeComposition
