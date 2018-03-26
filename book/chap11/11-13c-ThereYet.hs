-- 11-13c-ThereYet.hs
--
-- 11.13 Constructing and deconstructing values, page 421
-- Example, page 432
--
module ThereYet where

-- Works the same as if we'd used record syntax.
data ThereYet = There Float Int Bool
                deriving (Eq, Show)

-- who needs a "builder pattern"?
nope :: Float -> Int -> Bool -> ThereYet
nope = undefined

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False
