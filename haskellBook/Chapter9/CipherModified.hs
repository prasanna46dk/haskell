module Cipher where
import Data.Char
import Data.String
import System.IO

caeser :: String -> Int -> String
caeser [] no = []
caeser (x:xs) no
  | isUpper x = cipher 65
  | isLower x = cipher 97
  where
    cipher start =
      chr (((ord x + no) - start) `mod` 26 + start) : caeser xs no

main :: IO()
main =do
  hSetBuffering stdout NoBuffering
  putStr "Please enter string to encode: "
  rawStr <- getLine
  putStr "Please enter number to encdoe: "
  no <- getLine
  putStrLn $ caeser rawStr $ read no
