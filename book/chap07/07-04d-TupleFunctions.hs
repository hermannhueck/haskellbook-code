-- 07-04d-TupleFunctions.hs
--
-- 7.4 Pattern matching, page 226
-- Matching Tuples, page 234
--
module TupleFunctions where

-- These have to be the same type because
-- (+) is a -> a -> a
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

test = addEmUp2 (10, 20) == 30
  && addEmUp2Alt (10, 20) == 30
  && fst3 ("blah", 2, []) == "blah"
  && third3 ("blah", 2, [] :: [Int]) == ([] :: [Int])
  -- must specify the type of the empty lists when using ==
