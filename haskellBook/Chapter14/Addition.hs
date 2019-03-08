module Addition where

import Test.Hspec

--main :: IO ()
--main = hspec $ do
--  describe "Addition" $ do
--    it "1 + 1 is greater than 1" $ do
--      ((1 + 1) > (1::Integer)) `shouldBe` True
--    it "2 + 2 is equal to 4" $ do
--      2 + 2 `shouldBe` (4::Integer)

mulUsingRecursiveSummation :: (Integral a) => a -> a -> a
mulUsingRecursiveSummation x y
  | x == 0 = 0
  | otherwise = y + mulUsingRecursiveSummation (x-1) y

--dividedBy :: Integral a => a -> a -> (a, a)
--dividedBy num denom = go num denom 0
--  where
--    go n d count
--      | n < d = (count, n)
--      | otherwise = go (n - d) d (count + 1)

--main :: IO ()
--main = hspec $ do
--  describe "Addition" $ do
--    it "15 divided by 3 is 5" $ do
--      dividedBy 15 3 `shouldBe` (5, 0)
--    it "22 divided by 5 is\
--       \ 4 remainder 2" $ do
--      (dividedBy 22 5) `shouldBe` (4::Integer , 2::Integer)

main :: IO ()
main = hspec $ do
  describe "Mutliplication" $ do
    it "15 multiplied 3 is 45" $ do
      mulUsingRecursiveSummation 15 3 `shouldBe` (45:: Integer)
