-- 08-06e-McCarthy91.hs
--
-- 8.6 Chapter Exercises, page 294
-- McCarthy 91 function, page 296
--
module McCarthy91 where

mc91 :: (Ord b, Num b) => b -> b
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) (n + 11)

test = map mc91 [95..110] == [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]