-- 03-08a-ReadingSyntax.hs
--
-- 3.8 Chapter Exercises, page 81
-- Reading Syntax, page 76
--
module ReadingSyntax where

-- 1. Check if the expressions are written correctly
--    Correct those which are not correct.

-- 1a.   concat [[1,2,3], [4,5,6]] -- correct
e1a = concat [[1,2,3], [4,5,6]]
    
-- 1b.   ++ [1,2,3] [4,5,6] -- not correct
e1b = (++) [1,2,3] [4,5,6]

-- 1c.   (++) "hello" " world" -- correct
e1c = (++) "hello" " world"

-- 1d.   ["hello" ++ " world] -- not correct
e1d = ["hello" ++ " world"]

-- 1e.   4 !! "hello" -- nor correct
e1e = "hello" !! 4

-- 1f.   (!!) "hello" 4 -- correct
e1f = (!!) "hello" 4

-- 1g.   take "4 lovely" -- not correct
e1g = take 4 "lovely"

-- 1h.   take 3 "awesome" -- correct
e1h = take 3 "awesome"

-- 2. Next we have two sets:
--    The first set is a set of lines of code.
--    the second set is a set of results.
--    Associate the code expressions from the 1st set with the results in the 2nd set.

expr2a = concat[[1*6],[2*6],[3*6]]
expr2b = "rain" ++ drop 2 "elbow"
expr2c = 10 * head [1,2,3]
expr2d = (take 3 "Julie") ++ (tail "yes")
expr2e = concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]

result2a = "Jules"
result2b = [2,3,5,6,8,9]
result2c = "rainbow"
result2d = [6,12,18]
result2e = 10

test = expr2a == result2d
    && expr2b == result2c
    && expr2c == result2e
    && expr2d == result2a
    && expr2e == result2b

