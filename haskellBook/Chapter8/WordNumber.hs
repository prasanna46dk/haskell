module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"

digits :: Int -> [Int]
digits n =  digitList n []
  where
    digitList n lst =
      do
        let tupDivMod = n `divMod` 10
        case (fst tupDivMod == 0) of
          True -> [snd tupDivMod] ++ lst
          False -> [snd tupDivMod] ++ digitList (fst tupDivMod) lst
--  where
--    digitList n lst
--      | n < 10 = [n] ++ lst
--      | otherwise =
--          do
--            let lst' = [ n `mod` 10 ] ++ lst
--            digitList (n `div` 10) lst'
--
wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
