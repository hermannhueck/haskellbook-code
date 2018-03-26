-- 06-14e-TypeKwonDoTwo.hs
--
-- 6.14 Chapter Exercises, page 206
-- Type-Kwon-Do Two: Electrc Typealoo, page 211
--
module TypeKwonDoTwo where

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y
-- e.g.:   chk (*2) 2 4     -- yields True
-- e.g.:   chk (*2) 2 5     -- yields False

-- 2.
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = f x + fromInteger i
-- e.g.:   arith read 10 "5"     -- yields 15

-- test
test = chk (*2) 2 4 == True
  && chk (*2) 2 5 == False
  && arith read 10 "5" == 15
  