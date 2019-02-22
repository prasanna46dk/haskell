-- | This is wrong logic. There is bug in caeser
module VigenereCipher where

import Data.Char
import Data.List

type EncKey = String
type OrdEncKey = Int

-- | creates list of length of words in line
partitionWords:: String -> [Int]
partitionWords = map length . words

-- | generates list of encryption keys according to
-- | list of length provided by user.
encKeyList :: EncKey -> [Int] -> [EncKey]
encKeyList [] _ = []
encKeyList _ [] = []
encKeyList encKey (x:xs) =
  [takeX] ++ encKeyList dropX xs
  where
    takeX = take x infiEncKey
    dropX = drop x infiEncKey
    infiEncKey = concat $ repeat encKey

-- | converts encKeyList to ascii values of chars
ordEncKeyList :: [EncKey] -> [[OrdEncKey]]
ordEncKeyList [] = []
ordEncKeyList [[]] = [[]]
ordEncKeyList (x:xs) = map ord x : ordEncKeyList xs

vigenereCipher :: [String] -> [[Int]] -> [String]
vigenereCipher [] _ = []
vigenereCipher _ [[]] = []
vigenereCipher (x:xs) (y:ys) =
  ([caeser x y] ++  vigenereCipher xs ys)

caeser :: String -> [Int] -> String
caeser [] [] = []
caeser [] _ = []
caeser _ [] = []
caeser (x:xs) (y:ys)
  | isUpper x = cipher 65
  | isLower x = cipher 97
  | otherwise = cipher 0
  where
    cipher start =
      if start > 0 then
        chr (((ord x + y) - start) `mod` 26 + start) : caeser xs ys
      else
        x : caeser xs ys

main :: String -> String -> IO ()
main rawStr encKey =
  print $ intercalate " " $ vigenereCipher (words rawStr) $ ordEncKeyList $ encKeyList encKey $ partitionWords rawStr
