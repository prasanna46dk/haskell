module Scans where

fibs = takeWhile (<100) $ 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fact = take 10 $ 1 : scanl (\x y-> (y+1) * x) 1 [1..]

--scanl (*) 1 [1..10]
