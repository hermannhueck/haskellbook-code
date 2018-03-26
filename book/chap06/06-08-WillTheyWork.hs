-- 06-08-WillTheyWork.hs
--
-- 6.8 Ord, page 186
-- Exercises: Will They Work?, page 170
--
module WillTheyWork where

-- 1.
e1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

-- 2.
e2 = compare (3 * 4) (3 * 5)

-- 3.
--    compare "Julie" True
--    doesn't work, as the args to compare have different types

-- 4.
e4 = (5 + 3) > (3 + 6)

-- test
test = e1 == 5 && e2 == LT && e4 == False
