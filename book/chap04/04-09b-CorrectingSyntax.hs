-- 04-09b-CorrectingSyntax.hs
--
-- 4.9 Chapter Exercises, page 109
-- Correcting Syntax, page 111
--
module CorrectingSyntax where

-- 1.
{- with syntax errors:
x = (+)

F xs = w 'x' 1
  where
    w = length xs
-}
-- fixed:
x = (+)

f xs = w `x` 1
  where
    w = length xs

-- 2. This is supposed to be the identity function id.
-- with syntax errors:
-- \X = x
-- fixed:
myId v = v
myId' = \v -> v

-- 3. When fixed, this function will return 1 from the value (1,2)
-- with syntax errors:
-- f (a b) = A
-- fixed:
myFst (a, b) = a