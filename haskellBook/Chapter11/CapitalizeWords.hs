module CapitalizeWords where

import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) 
  | isAlpha x = toUpper x : xs
  | otherwise = x : capitalizeWord xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
  createTuple $ words str
  where
    createTuple [] = []
    createTuple (x:xs) = (x , capitalizeWord x) : createTuple xs

splitPeriod :: String -> [String]
splitPeriod [] = [""]
splitPeriod (c:cs)
   | c == '.'  = "" : rest
   | otherwise = (c : head rest) : tail rest
 where
   rest = splitPeriod cs

capitalizePara :: String -> String
capitalizePara [] = []
capitalizePara xList@(x:xs) = intercalate "." $ map capitalizeWord $ splitPeriod xList

main :: Bool
main =
  capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]
