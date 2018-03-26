-- 09-09-MoreBottoms.hs
--
-- 9.9 Transforming lists of values, page 327
-- Exercises: More Bottoms, page 333
--
module MoreBottoms where

import Data.Bool (bool)

-- As always, we encourage you to try guring out
-- the answers before you enter them into your REPL.
-- 1. Will the following expression return a value or be ⊥?
e1 = take 1 $ map (+ 1) [undefined, 2, 3]

--    ⊥
-- 2. Will the following expression return a value?
e2 = take 1 $ map (+ 1) [1, undefined, 3]

--    v
test2 = ("test2", e2 == [2])

-- 3. Will the following expression return a value?
e3 = take 2 $ map (+ 1) [1, undefined, 3]

--    ⊥
-- 4. What does the following mystery function do?
-- What is its type? Describe it (to yourself or a loved one)
-- in standard English and then test it out
-- in the REPL to make sure you were correct.
itIsMystery xs = map (\x -> elem x "aeiou") xs

e4 = itIsMystery "abracadabra"

-- it maps the given string to a List of Bool with vowels being True and consonants being False
test4 = ( "test4" , e4 == [True, False, False, True, False, True, False, True, False, False, True])

-- 5. What will be the result of the following functions:
-- a)
e5a = map (^ 2) [1 .. 10]

-- returns the squares --> [1,4,9,16,25,36,49,64,81,100]
test5a = ("test5a", e5a == [1, 4, 9, 16, 25, 36, 49, 64, 81, 100])

-- b)
-- n.b. `minimum` is not the same function -- as the `min` that we used before
e5b = map minimum [[1 .. 10], [10 .. 20], [20 .. 30]]

-- returns the minima of the inner lists --> [1,10,20]
test5b = ("test5b", e5b == [1, 10, 20])

-- c)
e5c = map sum [[1 .. 5], [1 .. 5], [1 .. 5]]

-- returns the sums of the inner lists --> [15,15,15]
test5c = ("test5c", e5c == [15, 15, 15])

-- 6. Back in chapter 7, you wrote a function called foldBool.
-- That function exists in a module known as Data.Bool and is called bool.
-- Write a function that does the same (or similar, if you wish) as the
-- map (if-then-else) function you saw above but uses bool instead
-- of the if-then-else syntax. Your first step should be bringing
-- the bool function into scope by typing import Data.Bool at your Prelude prompt.
xs = [1..10]

foldBool :: (Eq a, Num a) => (a -> Bool) -> [a] -> [a]
foldBool p = map (\x -> if (p x) then (-x) else (x))

foldBool' :: (Eq a, Num a) => (a -> Bool) -> [a] -> [a]
foldBool' p = map (\x -> bool x (-x) (p x))

test6 = ("test6", foldBool (==3) xs == foldBool' (==3) xs)


tests = [test2, test4, test5a, test5b, test5c, test6]
