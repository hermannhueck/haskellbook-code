-- 04-09a-Exercises.hs
--
-- 4.9 Chapter Exercises, page 109
--
module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. type of length
-- length :: Foldable t => t a -> Int

-- 2.
test2a = length [1, 2, 3, 4, 5] == 5
test2b = length [(1, 2), (2, 3), (3, 4)] == 3
test2c = length allAwesome == 2
test2d = length (concat allAwesome) == 5

test2 = test2a && test2b && test2c && test2d

-- 3.
-- a) 6 / 3  -- works, both params of (/) are of type Fractional
expr3a = 6 / 3
-- b) 6 / length [1,2,3]  -- error, 1st param of (/) has type Fractional
--                                  2nd param of (/) has type Int, but should also be a Fractional

-- 4. 3b csan be fixed ...
-- a) using another operator
expr4a = 6 `div` (length [1,2,3])
-- b) using fromIntegral
expr4b = 6 / (fromIntegral (length [1,2,3]))

-- 5. What is the type of the expression: 2 + 3 == 5
--    --> Bool
--    What would we expect as a result?
--    --> True

-- 6. x = 5
--    What is the type of the expression: x + 3 == 5
--    --> Bool
--    What would we expect as a result?
--    --> False

-- 7. Below are some bits of code. Which will work? Why or why not?
--    If they will work, what value would they reduce to?
-- a) length allAwesome == 2
--    --> works and reduces to True
-- b) length [1, 'a', 3, 'b']
--    --> doesn't compile, because the list elements have different types.
-- c) length allAwesome + length awesome
--    --> works and reduces to 5
-- d) (8 == 8) && ('b' < 'a')
--    --> works and reduces to False
-- e) (8 == 8) && 9
--    --> doesn't compile as the 2nd arg to (&&) has not type Bool

-- 8. palindrome function
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9. implementation of abs
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

myAbs' :: Integer -> Integer
myAbs' x
  | x < 0 = (-x)
  | otherwise = x

-- 10. Fill in the definition of the following function using fst and snd
f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

-- same with pattern matching
f' :: (a,b) -> (c,d) -> ((b,d),(a,c))
f' (a,b) (c,d) = ((b,d), (a,c))
