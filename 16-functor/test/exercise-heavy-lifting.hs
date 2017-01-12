module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Heavy lifting" $ do
        it "a = (+1) $ read \"[1]\" :: [Int]" $ do
            (fmap (+1) $ (read "[1]" :: [Int])) `shouldBe` [2]

        it "b = (++ \"lol\") (Just [\"Hi,\", \"Hello\"])" $ do
            ((fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])) `shouldBe` Just ["Hi,lol", "Hellolol"]

        it "c = (*2) (\\x -> x - 2)" $ do
            let c = (*2) . (\x -> x - 2)
            (c 1 :: Int) `shouldBe` (-2)

        it "d = ((return '1' ++) . show) (\\x -> [x, 1..3])" $ do
            let d = ((return '1' ++) . show) . (\x -> [x, 1..3])
            (d (0 :: Int)) `shouldBe` "1[0,1,2,3]"

        it "e :: IO Integer" $ do
            let e = let ioi = read "1" :: Int
                        changed = read $ ("123"++) $ show ioi
                    in (*3) changed
            e `shouldBe` (3693 :: Int)
