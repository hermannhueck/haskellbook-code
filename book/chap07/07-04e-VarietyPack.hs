-- 07-04e-VarietyPack.hs
--
-- 7.4 Pattern matching, page 226
-- Exercises: Variety Pack, page 235
--
module VarietyPack where

-- 1. Given the following declarations
k (x, y) = x

k1 = k ((4 - 1), 10)

k2 = k ("three", (1 + 2))

k3 = k (3, True)

-- a) What is the type of k?
-- Answer: k :: (a, b) -> a

-- b) What is the type of k2
-- Answer: k2 :: [Char]
-- is it the same type as k1 or k3?
-- Answer: no

-- c) Of k1, k2, k3, which will return the result of 3?
-- Answer: k1 and k3

-- 2. Define the following function for the given type
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
