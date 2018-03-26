-- 04-09c-FunctionTypes.hs
--
-- 4.9 Chapter Exercises, page 109
-- Match the function names to their types, page 112
--
module FunctionTypes where

-- 1. Which of the following is the type of 'show'?

-- a) show a => a -> String
-- b) Show a -> a -> String
-- c) Show a => a -> String
-- Answer: c

-- 2. Which of the following is the type of (==)?

-- a) a -> a -> Bool
-- b) Eq a => a -> a -> Bool
-- c) Eq a -> a -> a -> Bool
-- d) Eq a -> A -> Bool
-- Answer: b

-- 3. Which of the following is the type of 'fst'?

-- a) (a, b) -> a
-- b) b -> a
-- c) (a, b) -> b
-- Answer: a

-- 4. Which of the following is the type of (+)?

-- a) (+) :: Num a -> a -> a -> Bool
-- b) (+) :: Num a => a -> a -> Bool
-- c) (+) :: num a => a -> a -> a
-- d) (+) :: Num a => a -> a -> a
-- e) (+) :: a -> a -> a
-- Answer: d

