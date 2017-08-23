module Main where

import Test.Hspec
import Test.QuickCheck

-- Some sample data.

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and y using z as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- Have x1 make a tuple of xs and ys, and x2 make a tuple of ys and zs.
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- Also, write x3 which takes one input and makes a tuple of the results of two
-- applications of z' from above.

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

-- Let’s use uncurry to allow us to add the two values that are inside a tuple:
summed :: Num c => (c, c) -> c
summed = uncurry (+)

main :: IO ()
main = hspec $ do
    describe "First steps" $ do
      it "zip x and y using 3 as the lookup key" $
          xs `shouldBe` Just 6
      it "zip y and z using 6 as the lookup key" $
          ys `shouldBe` Just 9
      it "zip x and y using 4 as the lookup key" $
          zs `shouldBe` Nothing
      it "z' 1 = Just 7" $
          z' 1 `shouldBe` Just 7
      it "z' 4 = Nothing" $
          z' 4 `shouldBe` Nothing
      it "x1 = Just (6, 9)" $
          x1 `shouldBe` Just (6, 9)
      it "x2 = Nothing" $
          x2 `shouldBe` Nothing
      it "x3 3 = (Just 9, Just 9)" $
          x3 3 `shouldBe` (Just 9, Just 9)
      it "summed works like sum" $
          property ((\n -> summed (n, n) == n + n) :: (Integer -> Bool))
