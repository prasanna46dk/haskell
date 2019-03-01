module Phone where
import Data.Char

data Button = Button(String, String) deriving (Eq, Show)
daPhone :: [Button]
daPhone = [ Button ("1","1")
           ,Button ("2","2abc")
           ,Button ("3","3def")
           ,Button ("4","4ghi")
           ,Button ("5","5jkl")
           ,Button ("6","6mno")
           ,Button ("7","7pqrs")
           ,Button ("8","8tuv")
           ,Button ("9","9wxyz")
           ,Button ("0","0+ ")
           ,Button ("*","*^")
           ,Button ("#","#.,")
          ]

fstButton :: Button -> String
fstButton (Button(x, _)) = x

sndButton :: Button -> String
sndButton (Button(_,y)) = y

searchButton :: String -> [Button] -> Button
searchButton key buttons = head [ y |  y <- buttons, key == (fstButton y)]

isCapital :: String -> Bool
isCapital [] = False
isCapital (x:[]) = False
isCapital (x:y:xs) =
  case x /= y && x == '*' of
    True -> True
    False -> False

getChar :: String -> [Button] -> String
getChar t@(x:xs) buttons =
  if (charIndex /= 0) && isCap then
    [toUpper char]
  else
    [char]
  where
    isCap = isCapital t
    t' = if isCapital t then xs else (x:xs)
    char = (sndButton but) !! charIndex
    but = searchButton ([head t']) buttons
    charIndex = (length t') `mod` (length $ sndButton but)
