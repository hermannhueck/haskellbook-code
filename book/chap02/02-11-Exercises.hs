-- 02-11-Exercises.hs
--
-- 2.11 Chapter Exercises, page 60
--
module Exercises where

--------------------------------------------
-- Parenthesization

-- 1.
e11a = 2 + 2 * 3 - 1
e11b = (2 + (2 * 3)) - 1

-- 2.
e12a = (^) 10 $ 1 + 1
e12b = (^) 10 $ (1 + 1)

-- 3.
e13a = 2 ^ 2 * 4 ^ 5 + 1
e13b = ((2 ^ 2) * (4 ^ 5)) + 1

test1 = e11a == e11b
    && e12a == e12b
    && e13a == e13b

--------------------------------------------
-- Equivalent expressions

-- 1. --> expresions are equivalent
e21a = 1 + 1
e21b = 2

-- 2. --> expresions are equivalent
e22a = 10 ^ 2
e22b = 10 + 9 * 10

-- 3. --> expresions are NOT equivalent
e23a = 400 - 37
e23b = (-) 37 400

-- 4. --> expresions are NOT equivalent
e24a = 100 `div` 3
e24b = 100 / 3

-- 5. --> expresions are NOT equivalent
e25a = 2 * 5 + 18
e25b = 2 * (5 + 18)

test2 = e21a == e21b
    && e22a == e22b
    && e23a /= e23b
    && fromIntegral e24a /= e24b
    && e25a /= e25b

--------------------------------------------
-- waxOn / waxOff

z = 7             -- > 7
x = y ^ 2         -- > 225
waxOn = x * 5     -- > 1125
y = z + 8         -- > 15

-- 1.
test31 = 10 + waxOn == 1135
    && (+10) waxOn == 1135
    && (-) 15 waxOn == (-1110)
    && (-) waxOn 15 == 1110

-- 2.
triple x = x * 3

-- 3.
test33 = triple waxOn == 3375

-- 4 + 5.
waxOn2 = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

test34 = waxOn2 == waxOn

-- 6.
waxOff x = triple x

test36 = waxOff waxOn == 3375
    && waxOff waxOn == triple waxOn

-- 7.
test37 = waxOff 10 == 30
    && waxOff (-50) == (-150)
