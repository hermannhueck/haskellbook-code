-- 09-08-BottomMadness.hs
--
-- 9.8 Spines and nonstrict evaluation, page 318
-- Exercises: Bottom Madness, page 326
--
module BottomMadness where

-- Will it blow up?
-- Will the following expressions return a value or be ⊥?

-- 1.    ⊥
e1_1 = [x^y | x <- [1..5], y <- [2, undefined]]
-- 2.    v
e1_2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- 3.    ⊥
e1_3 = sum [1, undefined, 3]
-- 4.    v
e1_4 = length [1, 2, undefined]
-- 5.    ⊥
e1_5 = length $ [1, 2, 3] ++ undefined
-- 6.    v
e1_6 = take 1 $ filter even [1, 2, 3, undefined]
-- 7.    ⊥
e1_7 = take 1 $ filter even [1, 3, undefined]
-- 8.    v
e1_8 = take 1 $ filter odd [1, 3, undefined]
-- 9.    v
e1_9 = take 2 $ filter odd [1, 3, undefined]
-- 10.    ⊥
e1_10 = take 3 $ filter odd [1, 3, undefined]

{-
Intermission: Is it in normal form?

For each expression below, determine whether it’s in:
1. normal form, which implies weak head normal form;
2. weak head normal form only; or,
3. neither.

Remember that an expression cannot be in normal form
or weak head normal form if the outermost part of the
expression isn’t a data constructor. It can’t be in
normal form if any part of the expression is unevaluated.
-}

-- 1.    WHNF && NF
e2_1 = [1, 2, 3, 4, 5]
-- 2.    BOTTOM
-- does not compile due to hole at the end
-- e2_2 = 1 : 2 : 3 : 4 : _
-- 3.    ---
e2_3 = enumFromTo 1 10
-- 4.    WHNF && NF
e2_4 = length [1, 2, 3, 4, 5]
-- 5.    WHNF && NF
e2_5 = sum (enumFromTo 1 10)
-- 6.    WHNF
e2_6 = ['a'..'m'] ++ ['n'..'z']
-- 7.    BOTTOM
-- does not compile due to hole at first tuple element
-- e2_7 = (_, 'b')
