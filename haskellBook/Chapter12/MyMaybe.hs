module MyMaybe where

-- | 1
myIsJust :: Maybe a -> Bool
myIsJust (Just _) = True
myIsJust _ = False

myIsNothing :: Maybe a -> Bool
myIsNothing (Just _) = False
myIsNothing Nothing = True

-- | 2
myFromJust :: Maybe a -> a
myFromJust (Just b) = b
myFromJust Nothing = error "Not Just value"

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def f maybe =
  case myIsJust maybe of
    True -> f $ myFromJust maybe
    False -> def

-- | 3
myFromMaybe :: a -> Maybe a -> a
myFromMaybe def maybe =
  case myIsJust maybe of
    True -> myFromJust maybe
    False -> def

-- | 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList maybe = [myFromJust maybe]

-- | 5
myCatMaybe :: [Maybe a] -> [a]
myCatMaybe [] = []
myCatMaybe (x:xs) =
  case myIsNothing x of
    True -> myCatMaybe xs
    False -> myFromJust x : myCatMaybe xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe maybeLst =
  case foldr (&&) True $ map myIsJust maybeLst of
    True -> Just $ map myFromJust maybeLst
    False -> Nothing

