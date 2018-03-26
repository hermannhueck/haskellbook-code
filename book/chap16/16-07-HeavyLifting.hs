-- 16-07-HeavyLifting.hs

-- 16.7 Commonly used functors, page 645
-- Exercises: Heavy Lifting, page 656

-- Add fmap, parentheses, and function composition to the expression as needed for the expression
-- to typecheck and produce the expected result. It may not always need to go in the same place,
-- so don’t get complacent.


module HeavyLifting where

import Test.QuickCheck


-- 1.   a = (+1) $ read "[1]" :: [Int]
-- Solution:
a = fmap (+1) $ (read "[1]" :: [Int])
-- Prelude> a
-- : [2]

-- 2.   b = (++ "lol") (Just ["Hi,", "Hello"])
-- Solution:
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Prelude> b
-- Just ["Hi,lol","Hellolol"]

-- 3.   c = (*2) (\x -> x - 2)
-- Solution:
c = (*2) . (\x -> x - 2)
-- Prelude> c 1
-- -2

-- 4.   d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- Solution:
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"

-- 5.
--  e :: IO Integer
--  e = let ioi = readIO "1" :: IO Integer
--          changed = read ("123"++) show ioi
--      in (*3) changed
-- Solution:
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed_uncomposed = (fmap (\x -> read x::Integer) . fmap ("123"++) . fmap show) ioi
        changed_composed = fmap ((\x -> read x::Integer) . ("123"++) . show) ioi
    in fmap (*3) changed_composed
-- Prelude> e
-- 3693
e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
     in fmap ((*3) . (\x -> read x::Integer) . ("123"++) . show) ioi
-- Prelude> e'
-- 3693
