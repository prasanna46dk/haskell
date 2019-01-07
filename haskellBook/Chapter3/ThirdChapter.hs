--ThirdChapter.hs
--
module StringManupulation where
addExlametoryMark :: [Char] -> [Char]
addExlametoryMark a = a ++ "!"

getFifth :: [Char] -> Char
getFifth a = a !! 4

drop9 :: [Char] -> [Char]
drop9 a = drop 9 a

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

reverseWords :: [Char] -> [Char]
reverseWords x = 
    drop 9 x ++ take 4 (drop 5 x) ++ take 5 x

main :: IO()
main = print(reverseWords "Curry is awesome")
