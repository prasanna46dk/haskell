module Subseqence where
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xList@(x:xs) yList@(y:ys)
  | x == y = True && isSubseqOf xs ys
  | otherwise = False || isSubseqOf xList ys
