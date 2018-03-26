-- 08-06a-ReviewTheTypes.hs
--
-- 8.6 Chapter Exercises, page 294
-- Review the types, page 294
--
module ReviewTheTypes where

{-
1. What's the type of [[True, False], [True, True], [False, True]]?
   a) Bool
   b) mostly True
   c) [a]
   d) [[Bool]]
Answer: d)
-}

{-
2. Which of the followinf has the same type as [[True, False], [True, True], [False, True]]?
   a) [(True, False), (True, True), (False, True)]
   b) [[3 == 3],[6 > 5],[3 < 4]]
   c) [3 == 3, 6 > 5, 3 < 4]
   d) ["Bool", "more Bool", "Booly Bool!"]
Answer: b)
-}

-- 3. For the following function
func :: [a] -> [a] -> [a]
func x y = x ++ y
{-
   Which of the following is true?
   a) x and y must be of the same type
   b) x and y must both be lists
   c) if x is a String then y must be a String
   d) all of the above
Answer: d)
-}

{-
4. For the func code above, which is a valid application of func for both of its args?
   a) func "Hello World"
   b) func "Hello" "World"
   c) func [1, 2, 3] "a, b, c"
   d) func ["Hello", "World"]
Answer: b)
-}
