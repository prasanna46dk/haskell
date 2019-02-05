module DataBaseExercise where
-- | Page No  568 on Ereader haskell book
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- | Write a function that filters for DbDate values and return a list of the UTCTime values inside them.
filterDate :: [DatabaseItem] -> [UTCTime]
filterDate db = foldr f [] db
  where 
    f (DbDate a) b = a : b
    f _ b = b

-- | Write a function that filters Db NUmber values and returns a list of integer vales inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr f [] db
  where 
    f (DbNumber a) b = a : b
    f _ b = b

-- | Write a function that gets the most recent data.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDate

-- | Write a function that sums all DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- | Write function that gets average DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb db = sum db / len db
  where 
    sum = fromIntegral . sumDb
    len = fromIntegral . length . filterDbNumber
