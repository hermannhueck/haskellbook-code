-- 12-05b-StringProcessing.hs
--
-- 12.5 Chapter Exercises, page 480
-- String Processing, page 480
--
module StringProcessing where

import Data.List
import Data.Char

-- Because this is the kind of thing linguists ahem enjoy doing in their spare time.

-- 1. Write a recursive function named replaceThe which takes a text/string,
-- breaks it into words and replaces each instance of “the” with “a”.
-- It’s intended only to replace exactly the word “the”.
-- notThe is a suggested helper function for accomplishing this.

-- example GHCi session
-- above the functions

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

theToA :: String -> String
theToA word =
    case notThe word of
        Nothing -> "a"
        Just s -> s

theToA2 :: String -> String
theToA2 "the" = "a"
theToA2 word = word
        
-- >>> replaceThe "the cow loves us" -- "a cow loves us"

-- solution with recursion
replaceThe :: String -> String
replaceThe = concat
                . intersperse " "
                . replaceThe'
                . words
    where
        replaceThe' :: [String] -> [String]
        replaceThe' [] = []
        replaceThe' (word:words) = theToA word : replaceThe' words    -- recursive invocation ot replaceThe'

-- solution with map
replaceThe2 :: String -> String
replaceThe2 = unwords . map theToA2 . words


-- 2. Write a recursive function that takes a text/string, breaks it into words,
-- and counts the number of instances of ”the” followed by a vowel-initial word.

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1

lowerVowels = "aeiou"
vowels = lowerVowels ++ map toUpper lowerVowels

isVowel :: Char -> Bool
isVowel c = elem c vowels

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel str = isVowel $ head str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = countTheBeforeVowel' 0 $ words str
        where
            countTheBeforeVowel' :: Integer -> [String] -> Integer
            countTheBeforeVowel' cnt [] = cnt
            countTheBeforeVowel' cnt (w:[]) = cnt
            countTheBeforeVowel' cnt (w1:w2:ws)
                    | w1 == "the" && startsWithVowel w2 = countTheBeforeVowel' (cnt+1) (w2:ws)
                    | otherwise                         = countTheBeforeVowel' cnt (w2:ws)


-- 3. Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps.
-- Add any helper functions necessary to achieve your objectives.

-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel
