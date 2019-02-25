module StringProcessing where

import Data.Maybe
import Data.List

-- | 1
notThe :: String -> Maybe String
notThe str =
  if str == "the" then
    Nothing
  else
    Just str

replaceThe :: [Maybe String] -> [Maybe String]
replaceThe [] = []
replaceThe (x:xs) =
  if x == Nothing then
    (Just "a") : replaceThe xs
  else
    x : replaceThe xs

replaceTheWithA :: String -> String
replaceTheWithA str =
  intercalate " " $ map fromJust maybeString
  where
    maybeString = replaceThe notTheStr
    notTheStr = map notThe strWords
    strWords = words str

-- | 2
vowels = "aeiou"
consonants = "bcdfghjklmnpqrstvwxyz"

countTheVowels :: [String] -> Int
countTheVowels [] = 0
countTheVowels (x:[]) = 0
countTheVowels (x:y:rest) =
  if (x == "the") && any (==(head y)) vowels then
    1 + countRest
  else
    0 + countRest
  where
    countRest = countTheVowels (y:rest)

countTheBeforeVowel :: String -> Int
countTheBeforeVowel str=
  countTheVowels strWords
  where
    strWords = words str

-- | 3
countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs) =
  if any (== x) vowels then
    1 + countVowels xs
  else
    0 + countVowels xs

-- | Validate the word
newtype Word' = Word' String deriving (Eq, Show)

countConsonants :: String -> Integer
countConsonants [] = 0
countConsonants (x:xs) =
  if any (== x) consonants then
    1 + countConsonants xs
  else
    0 + countConsonants xs

mkWord :: String -> Maybe Word'
mkWord str =
  if consonantCount < vowelCount then
    Nothing
  else
    Just $ Word' str
  where
    consonantCount = countConsonants str
    vowelCount = countVowels str

