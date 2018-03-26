-- 08-06c-DividedBy.hs
--
-- 8.6 Chapter Exercises, page 294
-- Fixing dividedBy, page 296
--
module DividedBy where

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy num denom
  | denom == 0 = Nothing
  | num == 0 = Just (0, 0)
  | otherwise = go (abs num) (abs denom) 0 (signum num) (signum denom)
  where
    go :: Integral a => a -> a -> a -> a -> a -> Maybe (a, a)
    go n d count sn sd
          | n < d = Just (count * sn * sd, n * sn)
          | otherwise = go (n - d) d (count + 1) sn sd

test = dividedBy 10 2 == Just (5, 0)
  && dividedBy 10 (-2) == Just (-5, 0)
  && dividedBy (-10) (-2) == Just (5, 0)
  && dividedBy (-10) 2 == Just (-5, 0)
  && dividedBy 10 3 == Just (3, 1)
  && dividedBy 10 (-3) == Just (-3, 1)
  && dividedBy (-10) (-3) == Just (3, -1)
  && dividedBy (-10) 3 == Just (-3, -1)
  && dividedBy 10 0 == Nothing
