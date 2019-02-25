module Phone where

data Button = Button(String, String) deriving (Eq, Show)
daPhone :: [Button]
daPhone = [ Button ("1","1")
           ,Button ("2","2ABC")
           ,Button ("3","3DEF")
           ,Button ("4","4GHI")
           ,Button ("5","5JKL")
           ,Button ("6","6MNO")
           ,Button ("7","7PQRS")
           ,Button ("8","8TUV")
           ,Button ("9","9WXYZ")
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

getChar :: String -> [Button] -> String
getChar t@(x:xs) buttons =
  do
    let but = searchButton ([head t]) buttons
    let charIndex = (length t) `mod` (length $ sndButton but)
    return $ (sndButton but) !! charIndex

