-- Chapter 4.0
-- Excersise: Mood Swing
data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
