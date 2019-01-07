-- | Exercies: Eq Instances => Page Number 178
-- | Question 1

data TisAnInteger = 
  TisAn Integer 

instance Eq (TisAnInteger) where 
  (==) (TisAn a) (TisAn a') = 
    compare a a' == EQ

-- | Question 2

data TwoInteger = 
  Two Integer Integer

instance Eq (TwoInteger) where
  (==) (Two a a') (Two b b') = 
    (compare a b == EQ) && (compare a' b' == EQ)

-- | Question 3

data StringOrInt = 
    TisAnInt Int
  | TisAString String

instance Eq (StringOrInt) where
  (==) (TisAnInt a) (TisAnInt b) = 
    compare a b == EQ
  (==) (TisAString a) (TisAString b) = 
    compare a b == EQ

-- | Question 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = 
    (a == a') && (b == b')
      
-- | Question 5

data Tuple a b = 
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple s d) =
    a == s && b == d
-- | Question 6

data Which a = 
    ThisOne a 
  | ThatOne a 

instance Eq a => Eq (Which a) where 
    (==) (ThisOne a) (ThisOne b) = a == b
        
-- | Question 7

data EitherOr a b = 
    Hello a 
  | Goodbye b 

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello c) = a == c
    (==) (Goodbye b) (Goodbye d) = b == d

-- | Chapter Exersise 
-- | Page 207 .. Question 1 
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- | Page 208 .. Question 2 
data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

