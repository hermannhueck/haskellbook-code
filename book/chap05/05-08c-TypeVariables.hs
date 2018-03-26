-- 05-08c-TypeVariables.hs
--
-- 5.8 Chapter Exercises, page 147
-- Type variable or specific type constructor?, page 150
--
module TypeVariables where

-- You will be shown a type declaration, and you should categorize
-- each type. The choices are:
--
-- fp = fully polimprphic type variable
-- cp = constrained polimprphic type variable
-- ct = concrete type constructor

-- 1.
f1 :: Num a => a -> b -> Int -> Int
--             cp   fp   ct     ct
f1 = undefined

-- 2.
f2 :: zed -> Zed -> Blah
--    fp     ct     ct
f2 = undefined

-- 3.
f3 :: Enum b => a -> b -> C
--              fp   cp   ct
f3 = undefined

-- 4.
f4 :: f -> g -> C
--    fp   fp   ct
f4 = undefined
