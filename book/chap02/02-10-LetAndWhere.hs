-- 02-10-LetAndWhere.hs

-- 2.10 Let And Where, page 39

module LetAndWhere where

printInc n =
  print plusTwo
  where plusTwo = n + 2

printInc2 n =
  let plusTwo = n + 2
  in print plusTwo