module ChapterTenExercise where
-- | 1. Given the following sets of consonants and vowels:
-- |    stops = "pbtdkg"
-- |    vowels = "aeiou"
-- |    a) Write a function that takes inputs from stops and
-- |       vowels and makes 3-tuples of all possible stop-vowelstop
-- |       combinations. These will not all correspond to
-- |       real words in English, although the stop-vowel-stop
-- |       pattern is common enough that many of them will.
-- |    b) Modify that function so that it only returns the combinations that begin with a p.
-- |    c) Now set up lists of nouns and verbs (instead of stops
-- |       and vowels) and modify the function to make tuples
-- |       representing possible noun-verb-noun sentences

stops = "pbtdkg"
vowels = "aeiou"

threeTuples :: [(Char, Char, Char)]
threeTuples = [(x,y,z) | x <- stops, z <- stops, y <- vowels]

pTuples :: [(Char, Char, Char)]
pTuples = [(x,y,z) | x <- stops, z <- stops, y <- vowels, x == 'p', z /= x]

-- | 2. What does the following mystery function do? What is
-- |    its type? Try to get a good sense of what it does before
-- |    you test it in the REPL to verify it.
-- |    seekritFunc x =
-- |    div (sum (map length (words x)))
-- |    (length (words x))
seekritFunc :: String -> Int
-- | Number of alphabates in word (avg) 
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- |We’d really like the answer to be more precise. Can you
-- |rewrite that using fractional division?

seekritFunc' :: String -> Double
seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- | Rewriting functions using folds
-- | In the previous chapter, you wrote these functions using direct
-- | recursion over lists. The goal now is to rewrite them using folds.
-- | Where possible, to gain a deeper understanding of folding, try
-- | rewriting the fold version so that it is point-free.
-- | Point-free versions of these functions written with a fold
-- | should look like:
-- | myFunc = foldr f z
-- | So for example with the and function:
-- | Again, this type will be less
-- | reusable than the one in GHC 7.10
-- | and newer. Don't worry.
-- | 
-- | direct recursion, not using (&&)
-- | myAnd :: [Bool] -> Bool
-- | myAnd [] = True
-- | myAnd (x:xs) =
-- |   if x == False
-- |   then False
-- |   else myAnd xs
-- | 
-- | direct recursion, using (&&)
-- |   
-- | myAnd :: [Bool] -> Bool
-- |   myAnd [] = True
-- |   myAnd (x:xs) = x && myAnd xs
-- | fold, not point-free
-- | in the folding function
-- | myAnd :: [Bool] -> Bool
-- | myAnd = foldr
-- | (\a b -> if a == False then False else b) True
-- | 
-- | fold, both myAnd and the folding
-- | function are point-free now
-- | 
-- | myAnd :: [Bool] -> Bool
-- | myAnd = foldr (&&) True
-- | 
-- | The goal here is to converge on the final version where
-- | possible. You don’t need to write all variations for each example, 
-- | but the more variations you write, the deeper your
-- | understanding of these functions will become.
-- | 1. myOr returns True if any Bool in the list is True.
-- |    myOr :: [Bool] -> Bool
-- |    myOr = undefined

myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a == True then True else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

-- | 2. myAny returns True if a -> Bool applied to any of the values
-- |    in the list returns True.
-- |    myAny :: (a -> Bool) -> [a] -> Bool
-- |    myAny = undefined
-- |    Example for validating myAny:
-- |    Prelude> myAny even [1, 3, 5]
-- |    False
-- |    Prelude> myAny odd [1, 3, 5]
-- |    True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False
  
-- | 3. Write two versions of myElem. One version should use
-- |    folding and the other should use any.
-- |    myElem :: Eq a => a -> [a] -> Bool
-- |    Prelude> myElem 1 [1..10]
-- |    True
-- |    Prelude> myElem 1 [2..10]
-- |    False
myElem :: Eq a => a -> [a] -> Bool
myElem z xs = foldr (\x y -> z == x || y) False xs

-- | 4. Implement myReverse, don’t worry about trying to make
-- |    it lazy.
-- |    myReverse :: [a] -> [a]
-- |    myReverse = undefined
-- |    Prelude> myReverse "blah"
-- |    "halb"
-- |    Prelude> myReverse [1..5]
-- |    [5,4,3,2,1]
-- myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

-- | 5. Write myMap in terms of foldr. It should have the same
-- |    behavior as the built-in map.
-- |    myMap :: (a -> b) -> [a] -> [b]
-- |    myMap = undefined
myMap :: (a -> b) -> [a] -> [b] 
myMap f lst = foldr  
-- | 6. Write myFilter in terms of foldr. It should have the same
-- | behavior as the built-in filter.
-- | myFilter :: (a -> Bool) -> [a] -> [a]
-- | myFilter = undefined
-- | 7. squish flattens a list of lists into a list
-- | squish :: [[a]] -> [a]
-- | squish = undefined
-- | 8. squishMap maps a function over a list and concat
-- | CHAPTER 10. DATA STRUCTURE ORIGAMI 585
-- | squishMap :: (a -> [b]) -> [a] -> [b]
-- | squishMap = undefined
-- | Prelude> squishMap (\x -> [1, x, 3]) [2]
-- | [1,2,3]
-- | Prelude> f x = "WO " ++ [x] ++ " OT "
-- | Prelude> squishMap f "blah"
-- | "WO b OT WO l OT WO a OT WO h OT "
-- | 9. squishAgain flattens a list of lists into a list. This time re-use
-- | the squishMap function.
-- | squishAgain :: [[a]] -> [a]
-- | squishAgain = undefined
-- | 10. myMaximumBy takes a comparison function and a list and
-- | returns the greatest element of the list based on the last
-- | value that the comparison returned GT for.
-- | myMaximumBy :: (a -> a -> Ordering)
-- | -> [a]
-- | -> a
-- | myMaximumBy = undefined
-- | Prelude> myMaximumBy (\_ _ -> GT) [1..10]
-- | 1
-- | Prelude> myMaximumBy (\_ _ -> LT) [1..10]
-- | CHAPTER 10. DATA STRUCTURE ORIGAMI 586
-- | 10
-- | Prelude> myMaximumBy compare [1..10]
-- | 10
-- | 11. myMinimumBy takes a comparison function and a list and
-- | returns the least element of the list based on the last value
-- | that the comparison returned LT for.
-- | myMinimumBy :: (a -> a -> Ordering)
-- | -> [a]
-- | -> a
-- | myMinimumBy = undefined
-- | Prelude> myMinimumBy (\_ _ -> GT) [1..10]
-- | 10
-- | Prelude> myMinimumBy (\_ _ -> LT) [1..10]
-- | 1
-- | Prelude> myMinimumBy compare [1..10]
-- | 1
