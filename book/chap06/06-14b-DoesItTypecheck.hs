-- 06-14b-DoesItTypecheck.hs
--
-- 6.14 Chapter Exercises, page 206
-- Does it typecheck, page 207
--
module DoesItTypecheck where

-- 1.
-- data Person = Person Bool -- no instance of Show
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
-- data Mood = Blah | Woot deriving Show -- no instance of Eq
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

-- 3.
-- If you were able to get settleDown to typecheck
-- a) What values are acceptable inputs to that function?
--    Answer: Blah and Woot
-- b) What will happen if you try to run 'settleDown 9'? Why?
--    Answer: This expression doesn't typecheck.
--            The arg 9 is not a type constructor for Mood.
-- c) What will happen if you run 'Blah > Woot'? Why?
--    Answer: This expression doesn't typecheck.
--            Mood has no instance of Ord.
--            That can be fixed by deriving Mood also from Ord.
--            In that case the result would be False.

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
-- s1 typechecks. s1 is a function awaiting an Object (s1 :: Object -> Sentence)

s2 = Sentence "Julie" "loves" "dogs"
-- s2 typechecks. s2 is a Sentence (s2 :: Sentence)