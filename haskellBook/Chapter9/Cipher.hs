module Cipher where
import Data.Char

caeser :: String -> Int -> String
caeser [] no = []
caeser (x:xs) no
  | isUpper x = cipher 65
  | isLower x = cipher 97
  where
    cipher start =
      chr (((ord x + no) - start) `mod` 26 + start) : caeser xs no
