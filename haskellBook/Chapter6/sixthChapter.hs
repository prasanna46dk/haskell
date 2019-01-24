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

-- | Page 208 .. Question 4
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- | Page 208 ..
-- | Given datatype declaration, what can we do?
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- | Page 209
-- | Which of the following will typecheck? For the ones that don't typecheck, why don't they?

-- | 1.
-- | phew = Papu "Chases" True
-- |
-- | Answers :- Pappu :: Rocks -> Yeah -> Papu but here it is [Char] -> Bool Error.

-- | 2.
truth = Papu (Rocks "chomkydoz") (Yeah True)

-- | 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- | 4.
-- | comparePapus :: Papu -> Papu -> Bool
-- | comparePapus p p' = p > p'
-- |
-- | Answers :- Papu does not derive Ord instance. Error

-- | Match the types.
-- a)
i :: Num a => a
i = 1
--b)
--i :: a
