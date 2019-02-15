
data GuessWhat = 
  Checkenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b = 
  Product a b deriving (Eq, Show)

data Sum a b=
  First a | Second b deriving (Eq, Show)

data RecordProduct a b = 
  RecordProduct { pfirst :: a
                , psecond :: b } 
                deriving (Eq, Show)
