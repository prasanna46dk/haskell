module RemoveElementFromList where

import qualified Data.Set as Set
import qualified Data.List.Unique as U
removeElem :: (Eq a) => a -> [a] -> [a]
removeElem itm [] = []
removeElem itm lst =
  case itm == headOfList of
    False -> [headOfList] ++ removeElem itm tailList
    True -> tailList
  where
    headOfList = head lst
    tailList = tail lst

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates lst = length lst /= length set
  where set = Set.fromList lst
