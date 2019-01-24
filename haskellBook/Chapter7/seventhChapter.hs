-- | Exercise : Grab Bag .... Page 224
mTh x y z = x * y * z
nTh = \x -> \y -> \z -> x * y * z
jTh x y = \z -> x * y * z


-- | a) Page 225
--
addOneIfOdd = \n -> case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- | b)
addFive = \x -> \y -> (if x > y then y else x) + 5


-- | c)
mflip f x y = f y x

--Chapter Exercise: Page 404

--Lets write code
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit1 :: Integral a => a -> a
tensDigit1 x = d
  where
    xLastTup = x `divMod` 10
    d = fst xLastTup `mod` 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = x `div` 100
        d = xLast `mod` 10

-- | 2)
foldBool :: a -> a -> Bool -> a
foldBool x y bool =
  case bool of
    False -> x
    True -> y


-- |
foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool == False = x
  | otherwise = y

-- 3)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a,b) = (f a, b)

-- 4)
--
read' :: Read a => String -> a
show' :: Show a => a -> String
