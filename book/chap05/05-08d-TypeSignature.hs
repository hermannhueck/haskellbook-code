-- 05-08d-TypeSignature.hs
--
-- 5.8 Chapter Exercises, page 147
-- Write a type signature, page 151
--
module TypeSignature where

-- 1.
functionH :: [a] -> a
functionH (x:_) = x

-- 2.
functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y