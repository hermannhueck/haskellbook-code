-- 11-14-TheQuad.hs
--
-- 11.14 Function type is exponential, page 435
-- Exercise, page 440
--
module TheQuad where

-- Determine how many unique inhabitants each type has.
-- Suggestion: do the arithmetic unless you want to verify.
-- Writing them out gets tedious quickly.

data Quad =
    One
    | Two
    | Three
    | Four
    deriving (Eq, Show)


-- 1. How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = undefined
-- Answer: 4 + 4 = 8

-- 2. How many different forms can this take?
prodQuad :: (Quad, Quad)
prodQuad = undefined
-- Answer: 4 * 4 = 16

-- 3. How many different forms can this take?
funcQuad :: Quad -> Quad
funcQuad = undefined
-- Answer: 4 ^ 4 = 256

-- 4. How many different forms can this take?
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- Answer: 2 * 2 * 2 = 8

-- 5. How many different forms can this take?
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- Answer: (2 ^ 2) ^ 2 = 2 ^ (2 * 2) = 2 ^ 4 = 16

-- 6. How many different forms can this take?
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- Answer: (4 ^ 4) ^ 2 = 4 ^ (4 * 2) = 4 ^ 8 = 65536
