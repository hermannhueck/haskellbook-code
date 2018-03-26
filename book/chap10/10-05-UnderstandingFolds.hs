-- 10-05-UnderstandingFolds.hs
--
-- 10.5 Fold left, page 360
-- Exercises: Understanding Folds, page 365
--
module UnderstandingFolds where


-- 1.
foldr (*) 1 [1..5]          -- 120
-- will return the same result as which of the following:
--    a)
flip (*) 1 [1..5]           -- error
-- X  b)
foldl (flip (*)) 1 [1..5]   -- == 120
-- X  c)
foldl (*) 1 [1..5]          -- == 120


-- 2. Write out the evaluation steps for
foldl (flip (*)) 1 [1..3]   -- == 6
{-
foldl (flip (*)) 1 [1..3]
 = foldl (flip (*)) (1 * 1) [2,3]
 = foldl (flip (*)) (2 * (1 * 1)) [3]
 = foldl (flip (*)) (3 * (2 * (1 * 1))) []
 = (3 * (2 * (1 * 1)))
 -}

-- 3. One difference between foldr and foldl is:
--    a) foldr, but not foldl, traverses the spine of a list from right to left
--    b) foldr, but not foldl, always forces the rest of the fold
-- X  c) foldr, but not foldl, associates to the right
--    d) foldr, but not foldl, is recursive

-- 4. Folds are catamorphisms, which means they are generally used to
-- X  a) reduce structure
--    b) expand structure
--    c) render you catatonic
--    d) generate in nite data structures

-- 5. The following are simple folds very similar to what you’ve already seen,
-- but each has at least one error. Please fix them and test in your REPL:
-- a)
foldr (++) ["woot", "WOOT", "woot"]      -- wrong
foldr (++) "" ["woot", "WOOT", "woot"]   -- fixed
-- b)
foldr max [] "fear is the little death"  -- wrong
foldr max 'a' "fear is the little death" -- fixed -- use (chr 0) instead of 'a'
-- c)
foldr and True [False, True]    -- wrong
foldr (&&) True [False, True]   -- fixed
-- d) This one is more subtle than the previous.
-- Can it ever return a different answer?
foldr (||) True [False, True]    -- wrong -- always returns the same value: True
foldr (||) False [False, True]   -- fixed
-- e)
foldl ((++) . show) "" [1..5]             -- wrong -- the zero value is not a number
foldl (++) "" $ map show [1..5]           -- fixed
foldl (\x y -> x ++ (show y)) "" [1..5]   -- fixed
-- f) foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr const 'a' [1..5]      -- wrong: type of z-value not equal to result type
foldr const 0 [1..5]        -- fixed
-- g)
foldr const 0 "tacos"       -- wrong: type of z-value not equal to result type
foldr const 'a' "tacos"     -- fixed
-- h) foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl (flip const) 0 "burritos"       -- wrong: type of z-value not equal to result type
foldl (flip const) 'x' "burritos"     -- fixed
-- i)
foldl (flip const) 'z' [1..5]       -- wrong: type of z-value not equal to result type
foldl (flip const) 0 [1..5]       -- fixed
