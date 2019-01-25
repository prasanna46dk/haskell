module Cipher where
import Data.Char

caeser :: String -> Int -> String
caeser (x:xs) no
  | ascii + no > 90 && ascii < 97 =  
  ascii = (ord x) 
