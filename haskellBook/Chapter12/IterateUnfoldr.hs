module IterateUnfoldr where
import Data.Maybe
-- | 1
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- | 2
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f b = fst tup : myUnfoldr f (snd tup)
  where
    tup = fromJust (f b)
-- | 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x ,f x))
