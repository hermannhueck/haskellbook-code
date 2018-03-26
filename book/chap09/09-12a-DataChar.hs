-- 09-12a-DataChar.hs
--
-- 9.12 Chapter Exercises, page 339
-- Data.Char, page 339
--
module DataChar where

import Data.Char (isUpper, toUpper)
import Data.Maybe (listToMaybe)


{-
These first few exercises are straightforward but will introduce you
to some new library functions and review some of what we’ve learned so far.
Some of the functions we will use here are not standard in Prelude
and so have to be imported from a module called Data.Char.
You may do so in a source file (recommended) or at the Prelude prompt
with the same phrase: import Data.Char (write that at the top of your source file).
This brings into scope a bunch of new standard functions
we can play with that operate on Char and String types.
-}

--  1. Query the types of isUpper and toUpper.

--     Prelude Data.Char> :info isUpper
--     isUpper :: Char -> Bool
--     Prelude Data.Char> :info toUpper
--     toUpper :: Char -> Char

-- 2. Given the following behaviors, which would we use to write a function
-- that filters all the uppercase letters out of a String? Write that function
-- such that, given the input "HbEfLrLxO," your function will return “HELLO.”

--     Prelude Data.Char> isUpper 'J'
--     True
--     Prelude Data.Char> toUpper 'j'
--     'J'

filterUpper = filter isUpper

test2 = filterUpper "HbEfLrLxO," == "HELLO"

-- 3. Write a function that will capitalize the first letter of a string
-- and return the entire string. For example, if given the argument
-- “julie,” it will return “Julie.”

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (c:cs) = toUpper c : cs

test3 = capitalizeFirst "julie" == "Julie"

-- 4. Now make a new version of that function that is recursive
-- such that if you give it the input “woot” it will holler back at you “WOOT”.
-- The type signature won’t change, but you will want to add a base case.

capitalizeAll :: String -> String
capitalizeAll = map toUpper

test4 = capitalizeAll "woot" == "WOOT"

-- 5. To do the final exercise in this section, we’ll need another standard
-- function for lists called head. Query the type of head and experiment
-- with it to see what it does. Now write a function that will capitalize
-- the first letter of a String and return only that letter as the result.

firstAsUpper :: String -> Maybe Char
firstAsUpper "" = Nothing
firstAsUpper cs = Just $ toUpper $ head cs

test5 = firstAsUpper "julie" == Just 'J'

-- 6. Cool. Good work. Now rewrite it as a composed function.

firstAsUpper' :: String -> Maybe Char
firstAsUpper' "" = Nothing
firstAsUpper' cs = Just . toUpper . head $ cs

test6 = firstAsUpper' "julie" == Just 'J'

-- 7. Then, for fun, rewrite it pointfree.

firstAsUpper'' :: String -> Maybe Char
firstAsUpper'' = fmap toUpper . listToMaybe

test7 = firstAsUpper'' "julie" == Just 'J'
