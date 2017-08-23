module Main where

import Test.Hspec

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
