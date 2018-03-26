-- 07-03-GrabBag.hs
--
-- 7.3 Anonymous functions, page 223
-- Exercises: Grab Bag, page 224
--
module GrabBag where

-- 1. Which (two or more) of the following are equivalent?
mTh_a x y z = x * y * z
mTh_b x y = \z -> x * y * z
mTh_c x = \y -> \z -> x * y * z
mTh_d = \x -> \y -> \z -> x * y * z
-- a, b and c are equivalent.
-- mTh_d has type: Integer -> Integer -> Integer -> Integer (with GHC 8.2.2)

-- 2. The type of mTh_X is: Num a => a -> a -> a -> a
-- Which is the type of: mTh_X 3
-- a) Integer -> Integer -> Integer
-- b) Num a => a -> a -> a -> a
-- c) Num a => a -> a
-- d) Num a => a -> a -> a
-- Answer:
--         d) for mTh_a, mTh_b, mTh_c
--         a) for mTh_d

-- 3. Rewrite with anonymous function syntax

-- example:
addOne x = x + 1
addOne' = \x -> x + 1

-- a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addOneIfOdd'' = \n -> case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b)
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

addFive'' = \x y -> (if x > y then y else x) + 5

-- c) Rewrite without using anonymous lambda syntax
mflip f = \x -> \y -> f y x

mflip' f x y = f y x
