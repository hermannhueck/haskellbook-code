-- 17-09c-Ex3-Combinations.hs
--
-- 17.09 Chapter Exercises, page 741
-- Combinations, page 742
--
-- Remember the vowels and stops exercise in the folds chapter? Write the function
-- to generate the possible combinations of three input lists using liftA3 from Control.Applicative.
--
module Combinations where

import Control.Applicative (liftA3)

-- From chapter 10, Folding Lists:
-- Given the following sets of consonants and vowels:

stops  = "pbtdkg"
vowels = "aeiou"

-- Write a function that takes inputs from stops and vowels and makes 3-tuples
-- of all possible stop-vowel-stop combinations. These will not all correspond
-- to real words in English, although the stop-vowel-stop pattern is common
-- enough that many of them will.


-- 1. solution from chap 10 using a list comprehension
combis :: [a] -> [a] -> [(a, a, a)]
combis s v = [(x,y,z) | x <- s, y <- v, z <- s]

myCombis = combis stops vowels


-- solution with liftA3:
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\x y z -> (x, y, z))

myCombos = combos stops vowels stops


-- solution with <$> and <*>
combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' xs ys zs = (\x y z -> (x, y, z)) <$> xs <*> ys <*> zs

myCombos' = combos' stops vowels stops


-- all solutions should produce the same result:
testSolutions = myCombos == myCombis && myCombos' == myCombis
