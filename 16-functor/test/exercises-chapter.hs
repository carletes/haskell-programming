{-# LANGUAGE FlexibleInstances #-}

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

--- Flip f a b

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K a b =
    K a
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K (f a)

instance Arbitrary b => Arbitrary (Flip K a b) where
    arbitrary = do
        a <- arbitrary
        return $ Flip $ K a

flipIdentity :: Flip K Int Int -> Bool
flipIdentity = functorIdentity

flipComposition :: Flip K Int Int -> Fun Int Int -> Fun Int Int -> Bool
flipComposition = functorComposition

--- EvilGoateeConst a b

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = do
        b <- arbitrary
        return $ GoatyConst b

goatyIdentity :: EvilGoateeConst Int Int -> Bool
goatyIdentity = functorIdentity

goatyComposition :: EvilGoateeConst Int Int -> Fun Int Int -> Fun Int Int -> Bool
goatyComposition = functorComposition

--- LiftItOut f a

data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
    arbitrary = do
        a <- arbitrary
        fa <- oneof [return $ Just a,
                     return Nothing]
        return $ LiftItOut fa

liftItOutIdentity :: LiftItOut Maybe Int -> Bool
liftItOutIdentity = functorIdentity

liftItOutComposition :: LiftItOut Maybe Int -> Fun Int Int -> Fun Int Int -> Bool
liftItOutComposition = functorComposition

--- Parappa f g a =

data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary a) => Arbitrary (Parappa Maybe [] a) where
    arbitrary = do
        a <- arbitrary
        fa <- oneof [return $ Just a,
                     return Nothing]
        ga <- arbitrary
        return $ DaWrappa (fa) (ga)

parappaIdentity :: Parappa Maybe [] Int -> Bool
parappaIdentity = functorIdentity

parappaComposition :: Parappa Maybe [] Int -> Fun Int Int -> Fun Int Int -> Bool
parappaComposition = functorComposition

--- IgnoreOne f g a b

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe [] a b) where
    arbitrary = do
        a <- arbitrary
        fa <- oneof [return $ Just a,
                     return Nothing]
        gb <- arbitrary
        return $ IgnoringSomething fa gb

ignoreOneIdentity :: IgnoreOne Maybe [] Int Int -> Bool
ignoreOneIdentity = functorIdentity

ignoreOneComposition :: IgnoreOne Maybe [] Int Int -> Fun Int Int -> Fun Int Int -> Bool
ignoreOneComposition = functorComposition

--- Notorious g o a t

data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary t) => Arbitrary (Notorious Maybe (Maybe t) (Maybe t) t) where
    arbitrary = do
        go <- arbitrary
        ga <- arbitrary
        gt <- arbitrary
        return $ Notorious go ga gt

notoriousIdentity :: Notorious Maybe (Maybe Int) (Maybe Int) Int -> Bool
notoriousIdentity = functorIdentity

notoriousComposition :: Notorious Maybe (Maybe Int) (Maybe Int) Int -> Fun Int Int -> Fun Int Int -> Bool
notoriousComposition = functorComposition

--- List a

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        oneof [return Nil,
               return $ Cons x xs]

listIdentity :: List Int -> Bool
listIdentity = functorIdentity

listComposition :: List Int -> Fun Int Int -> Fun Int Int -> Bool
listComposition = functorComposition

-- GoatLord a

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
    arbitrary = do
        a <- arbitrary
        ga <- arbitrary
        gb <- arbitrary
        gc <- arbitrary
        oneof [return NoGoat,
               return $ OneGoat a,
               return $ MoreGoats ga gb gc]

goatLordIdentity :: GoatLord Int -> Bool
goatLordIdentity = functorIdentity

goatLordComposition :: GoatLord Int -> Fun Int Int -> Fun Int Int -> Bool
goatLordComposition = functorComposition

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

    describe "The functor for Flip K a b" $ do
        it "follows the identity law" $ do
            property flipIdentity

        it "follows the composition law" $ do
            property flipComposition

    describe "The functor for EvilGoateeConst a b" $ do
        it "follows the identity law" $ do
            property goatyIdentity

        it "follows the composition law" $ do
            property goatyComposition

    describe "The functor for liftItOut f a" $ do
        it "follows the identity law" $ do
            property liftItOutIdentity

        it "follows the composition law" $ do
            property liftItOutComposition

    describe "The functor for Parappa f g a" $ do
        it "follows the identity law" $ do
            property parappaIdentity

        it "follows the composition law" $ do
            property parappaComposition

    describe "The functor for IgnoreOne f g a b" $ do
        it "follows the identity law" $ do
            property ignoreOneIdentity

        it "follows the composition law" $ do
            property ignoreOneComposition

    describe "The functor for Notorious g o a t" $ do
        it "follows the identity law" $ do
            property notoriousIdentity

        it "follows the composition law" $ do
            property notoriousComposition

    describe "The functor for List a" $ do
        it "follows the identity law" $ do
            property listIdentity

        it "follows the composition law" $ do
            property listComposition

    describe "The functor for GoatLord a" $ do
        it "follows the identity law" $ do
            property goatLordIdentity

        it "follows the composition law" $ do
            property goatLordComposition
