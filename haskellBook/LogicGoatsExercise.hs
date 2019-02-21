{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
module Goats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany String where
  tooMany str = (length str) > 42

instance TooMany (Int, String) where
  tooMany (a, b) = tooMany (a + length b)

instance TooMany (Int, Int) where
  tooMany (a, b) = tooMany (a + b)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany(a + b)

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)
