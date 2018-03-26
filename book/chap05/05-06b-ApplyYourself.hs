-- 05-06b-ApplyYourself.hs
--
-- 5.6 Type Inference, page 142
-- Apply Yourself, page 145
--
module ApplyYourself where

-- 1.
-- Type signature of general function
-- (++) :: [a] -> [a] -> [a]
-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ "yo"
-- ==> myConcat :: [Char] -> [Char]

-- 2.
-- General function
-- (*) :: Num a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5
-- ==> myMult :: Fractional a => a -> a

-- 3.
-- take :: Int -> [a] -> [a]
myTake x = take x "hey you"
-- ==> myTake :: Int -> [Char]

-- 4.
-- (>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])
-- ==> myCom :: Int -> Bool

-- 5.
-- (<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'
-- ==> myAlph :: Char -> Bool
