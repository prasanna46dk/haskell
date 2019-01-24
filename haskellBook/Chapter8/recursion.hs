-- | Page 453 ---- Recursion
-- | 2)
sumN :: (Eq a, Num a) => a -> a
sumN n
  | n == 1 = 1
  | otherwise = n + sumN (n-1)

-- | 3)
mulUsingRecursiveSummation :: (Integral a) => a -> a -> a
mulUsingRecursiveSummation x y
  | x == 0 = 0
  | otherwise = y + mulUsingRecursiveSummation (x-1) y
