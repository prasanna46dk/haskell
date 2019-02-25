module NaturalsToInt where

import Data.Maybe

data Nat = Zero | Succ Nat deriving (Eq, Show)

predNat :: Nat -> Maybe Nat
predNat Zero = Nothing
predNat (Succ nat) = Just nat

integerToNat :: Int -> Maybe Nat
integerToNat int =
    case compare int 0 of
      LT -> Nothing
      EQ -> Just Zero 
      GT -> Just $ Succ $ fromMaybe Zero $ integerToNat (int - 1)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger nat = 1 + (natToInteger $ fromJust $ predNat nat)
