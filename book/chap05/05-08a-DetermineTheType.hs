-- 05-08a-DetermineTheType.hs
--
-- 5.8 Chapter Exercises, page 147
-- Determine the type, page 148
--

{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1

ex1a = (* 9) 6
-- 54 :: Num a => a
ex1b = head [(0,"doge"),(1,"kitteh")]
-- (0,"doge") :: Num a => (a, [Char])
ex1c = head [(0::Integer,"doge"),(1,"kitteh")]
-- (0,"doge") :: (Integer, [Char])
ex1d = if False then True else False
-- False :: Bool
ex1e = length [1,2,3,4,5]
-- 5 :: Int
ex1f = (length [1,2,3,4]) > (length "TACOCAT")
-- False :: Bool

x2 = 5
y2 = x2 + 5
w2 = y2 * 10
-- What is the type of w2?
-- Num a => a (value: 100)

x3 = 5
y3 = x3 + 5
z3 y3 = y3 * 10
-- What is the type of z3?
-- Num a => a -> a

x4 = 5
y4 = x4 + 5
f4 = 4 / y4
-- What is the type of f4?
-- Fractional a => a (value 0.4)

x5 = "Julie"
y5 = " <3 "
z5 = "Haskell"
f5 = x5 ++ y5 ++ z5
-- What is the type of f5?
-- [Char] (value: "Julie <3 Haskell")
