etfBool :: Bool -> Bool -> [Bool]
etfBool x y
  | x > y = []
  | x == y = [x]
  | x < y = x : etfBool (succ x) y
