-- 06-14d-MatchTheTypes.hs
--
-- 6.14 Chapter Exercises, page 206
-- Match the types, page 209
--
module MatchTheTypes where

import Data.List (sort)

-- 1.
i :: Num a => a
-- i :: a -- cannot replace the above type declaration
i = 1 -- literal 1 requires a Num

-- 2.
f2 :: Float
-- f2 :: Num a => a -- cannot replace the above type declaration
f2 = 1.0 -- literal 1.0 requires a Fractional

-- 3.
-- f3 :: Float -- can be replaced by ...
f3 :: Fractional a => a
f3 = 1.0

-- 4.
-- f4 :: Float -- can be replaced by ...
f4 :: RealFrac a => a
f4 = 1.0

-- 5.
-- freud :: a -> a -- can be replaced by ...
freud :: Ord a => a -> a
freud x = x

-- 6.
-- freud :: a -> a -- can be replaced by ...
freud' :: Int -> Int
freud' x = x

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a -- cannot replace the above type declaration
--                      myX is too rigid
sigmund x = myX

-- 8.
myX' = 1 :: Int

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a -- cannot replace the above type declaration
--                      myX' is too rigid
sigmund' x = myX'

-- 9. needs    import Data.List (sort)
-- jung :: Ord a => [a] -> a -- can be replaced by ...
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10.
-- young :: [Char] -> Char -- can be replaced by ...
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a -- cannot replace the above type declaration
--                                   mySort is too rigid; works only for [Char]
signifier xs = head (mySort xs)
