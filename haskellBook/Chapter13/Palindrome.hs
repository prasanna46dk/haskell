import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = getCharacter line1
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

getCharacter :: String -> String
getCharacter [] = []
getCharacter (x:xs) =
  case isAlpha x of
    True -> toLower x : rest
    False -> rest
  where 
    rest = getCharacter xs
