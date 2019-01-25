module StandardFunctions where
-- | Page 525
-- | 1)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

-- | 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
  if f x == True
  then True
  else myAny f xs

-- | 3)
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) =
  case a == x of
    True -> True
    False -> myElem a xs

-- | 4)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | 5)
squish :: [[a]] -> [a]
squish [] = []
squish [[]] = []
squish xs = head xs ++ squish (tail xs)

-- | 6)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- | 7)
--squishAgain :: [[a]] -> [a]
--squishAgain [] = []
--squishAgain [[]] = []
--squishAgain xs =
--

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x] = x
myMaximumBy f xs =
  case f (head xs) (head $tail xs) of
    GT -> myMaximumBy f ([(head xs)] ++  (tail $ tail xs))
    _ -> myMaximumBy f $tail xs

-- 9)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x] = x
myMinimumBy f xs =
  case f (head xs) (head $tail xs) of
    LT -> myMinimumBy f ([(head xs)] ++  (tail $ tail xs))
    _ -> myMinimumBy f $tail xs

