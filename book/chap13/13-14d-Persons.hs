-- 13-14-Ex4-Persons.hs
--
-- 13.14 Chapter Exercises, page 531
-- Modifying code, Persons, page 332
--
module Persons where

import System.IO

-- 4.

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

-- Your job is to write the following function gimmePerson WITHOUT modifying the code above.

-- Since IO () is about the least informative type imaginable, we’ll tell what it should do.
-- a) It should prompt the user for a name and age input.
-- b) It should attempt to construct a Person value using the name and age the user entered.
--    You’ll need the read function for Age because it’s an Integer rather than a String.
-- c) If it constructed a successful person, it should print ”Yay! Successfully got a person:” followed by the Person value.
-- d) If it got an error value, report that an error occurred and print the error.

gimmePerson :: IO ()
gimmePerson = do
    -- hSetBuffering stdout NoBuffering
    putStr "--- Enter the person's name: "
    name <- getLine
    putStr "--- Enter the person's age: "
    age <- getLine
    case mkPerson name (read age) of
        Right person -> print $ "Yay! Successfully got a person: " ++ show person
        Left error -> print $ "Got an error: " ++ show error
    return ()   -- return is optional
