findDigit :: Int -> [Int]
findDigit n = digitList n []
  where
    digitList n lst
      | n < 10 = [n] ++ lst
      | otherwise =
          do
            let lst' = [ n `mod` 10 ] ++ lst
            digitList (n `div` 10) lst'
