module MyEither where

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' (Right _) = False

isRight' :: Either a b -> Bool
isRight' (Left _) = False
isRight' (Right _) = True

getLeft' :: Either a b -> a
getLeft' (Right _) = error "No Left."
getLeft' (Left a) = a

getRight' :: Either a b -> b
getRight' (Right b) = b
getRight' (Left _) = error "No Right."

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) =
  case isLeft' x of
    True -> getLeft' x : rest
    False -> rest
  where
    rest = lefts' xs

rights' :: [Either a b] -> [b]
rights' [] = []
rights' (x:xs) =
  case isRight' x of
    True -> getRight' x : rest
    False -> rest
  where
    rest = rights' xs

-- | 3
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' eList = (lefts' eList, rights' eList)

-- | 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f either =
  case isRight' either of
    True -> Just $ f $ getRight' either
    False -> Nothing

-- | 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g either =
  case isLeft' either of
    True -> f $ getLeft' either
    False -> g $ getRight' either
