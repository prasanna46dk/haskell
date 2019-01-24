-- myWords :: String -> [String]
myWords str
  | str == "" = []
  | otherwise = [takeWhile (/= ' ') str] ++ myWords (dropWhile (== ' ') $ dropWhile (/= ' ') str)

myLines str
  | str == "" = []
  | otherwise = [takeWhile (/= '\n') str] ++ myLines (dropWhile (== '\n') $ dropWhile (/= '\n') str)

mySplit str sep
  | str == "" = []
  | otherwise = [takeWhile (/= sep) str] ++ mySplit (dropWhile      (== sep) (dropWhile (/= sep) str)) sep

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
  \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
  == shouldEqual)
