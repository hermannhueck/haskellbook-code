-- 07-07-GuardDuty.hs
--
-- 7.7 Guards, page 247
-- Exercises: Guard Duty, page 252
--
module GuardDuty where

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- 1. with the otherwise branch at the top position
avgGrade_1 :: (Fractional a, Ord a) => a -> Char
avgGrade_1 x
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  where
    y = x / 100

-- 2. guard expressions reordered
avgGrade_2 :: (Fractional a, Ord a) => a -> Char
avgGrade_2 x
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- 3. The following function returns
pal xs
  | xs == reverse xs = True
  | otherwise = False
-- b) True if xs is a palindrome

-- 4. Which types of arguments can 'pal' take?
-- Answer: a list of elements which have an Eq instance

-- 5. What is the type of the function 'pal'?
-- Answer: pal :: Eq a => [a] -> Bool

-- 6. The following function returns
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
-- c) an indication of whether it's argument is a positive
--    or negative number or zero.

-- 7. Which types of arguments can 'numbers' take?
-- Answer: any numeric value,
--    i.e. any arg that has an Num instance and an Ord instance

-- 8. What is the type of the function 'numbers'?
-- Answer: numbers :: (Ord a, Num a, Num p) => a -> p
