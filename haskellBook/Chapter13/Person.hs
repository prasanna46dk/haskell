module Person where

import System.IO
import Data.Either

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

--instance Show (PersonInvalid) where
--  show NameEmpty = "Name field should not be empty"
--  show AgeTooLow = "Age should be positive integer."
--  show (PersonInvalidUnknown str) = show str

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" && age > 0 = Left NameEmpty
  | not (age > 0) && name /= "" = Left AgeTooLow
  | otherwise =
       Left $ PersonInvalidUnknown $
         "Name was: " ++ show name ++
         " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please enter your name: "
  name <- getLine
  putStr "Please enter your age: "
  age <- getLine
  let person = mkPerson name $ read age
  case isRight person of
    True -> putStrLn $ "Yay! Successfully got a person: " ++ (show person)
    False -> putStrLn (show person)
