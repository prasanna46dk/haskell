import Data.Char

-- | 2)
isCapital :: [Char] -> [Char]
isCapital [] = []
isCapital (x:xs) =
  case isUpper x of
    True -> [x] ++ isCapital xs
    False -> isCapital xs

-- | 3)
capitaliseFirst :: [Char] -> [Char]
capitaliseFirst (x:xs) = (toUpper x ): xs

-- | 4)
capitaliseAll :: [Char] -> [Char]
capitaliseAll [] = []
capitaliseAll (x:xs) = toUpper x : capitaliseAll xs

-- 5)
capitaliseChar :: [Char] -> Char
capitaliseChar [] = ' '
capitaliseChar xs = toUpper $ head xs

-- 6)
capitaliseFirst' :: [Char] -> Char
capitaliseFirst' = toUpper . head
