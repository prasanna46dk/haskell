cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"
