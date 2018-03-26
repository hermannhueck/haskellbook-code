-- 04-03-MoodSwing.hs
--
-- 4.3 Anatomy of a data declaration, page 87
-- Exercises: Mood Swing, page 89
--
module MoodSwing where


data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah
