-- 08-06c-Recursion.hs
--
-- 8.6 Chapter Exercises, page 294
-- Recursion, page 295
--
module Recursion where

-- 1. Write out the steps for reducing dividedBy 15 2
--    to its final answer according to the Haskell code.
-- -- -- Won't do that here!!!

-- 2. Write a function that recursively sums all numbers from 1 to n
sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

-- 3. integral multiplication using recursive summation
integralMult :: (Integral a) => a -> a -> a
integralMult x y
  | x == 0 = 0
  | y == 0 = 0
  | y > 0 = x + integralMult x (y - 1)
  | otherwise = negate (x + integralMult x ((-y) - 1))



test = sumUpTo 5 == 15
  && integralMult 3 5 == 15
  && integralMult (-3) 5 == -15
  && integralMult 3 (-5) == -15
  && integralMult (-3) (-5) == 15
