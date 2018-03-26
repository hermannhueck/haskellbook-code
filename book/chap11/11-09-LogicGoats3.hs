-- 11-09-LogicGoats3.hs
--
-- 11.9 newtype, page 403
-- Exercises: Logic Goats, page 410
--
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats3 where

-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a).
-- This can mean whatever you want, such as summing the two numbers together.

-- solution with newtype

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') =  tooMany (n + n')



test = tooMany (42::Int, 1::Int)
