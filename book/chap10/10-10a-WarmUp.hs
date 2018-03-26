-- 10-10a-Ex1-WarmUp.hs
--
-- 10.10 Chapter Exercises, page 379
-- Warm up and review, page 379
--
module WarmUp where

-- For the following set of exercises, you are not expected to use folds.
-- These are intended to review material from previous chapters.
-- Feel free to use any syntax or structure from previous chapters that seems appropriate.

-- 1. Given the following sets of consonants and vowels:

stops  = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples
-- of all possible stop-vowel-stop combinations. These will not all correspond
-- to real words in English, although the stop-vowel-stop pattern is common
-- enough that many of them will.

combis :: [a] -> [a] -> [(a, a, a)]
combis stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- b) Modify that function so that it only returns the combinations that begin with a p.

combisWithP :: String -> String -> [(Char, Char, Char)]
combisWithP stops vowels = filter (\(x,y,z) -> x == 'p') (combis stops vowels)

-- c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify
-- the function to make tuples representing possible noun-verb-noun sentences.

nouns = ["dog", "cat", "elefant", "fly", "butterfly", "snake", "turtle"]
verbs = ["chases", "eats", "loves"]

-- 2. What does the following mystery function do? What is its type? Try to get
-- a good sense of what it does before you test it in the REPL to verify it.

seekritFunc x =
    div (sum (map length (words x)))
           (length (words x))

avgWordLength :: String -> Int
avgWordLength = seekritFunc


-- 3. We’d really like the answer to be more precise. Can you rewrite that using
-- fractional division?

avgWordLength' :: Fractional a => String -> a
avgWordLength' x =
    (/) (fromIntegral (sum (map length (words x))))
           (fromIntegral (length (words x)))
