module List where

myLast :: Eq a => [a] -> Maybe a
myLast [] = Nothing
myLast (x:xs) 
  | xs == [] = Just x
  | otherwise = myLast xs
