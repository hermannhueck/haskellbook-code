-- 11-09-LogicGoats2.hs
--
-- 11.9 newtype, page 406
-- Exercises: Logic Goats, page 410
--
module LogicGoats2 where

-- 2. Make another TooMany instance for (Int, Int). Sum the values together
-- under the assumption this is a count of goats from two fields.

-- solution with newtype

class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats (Int, Int) deriving Show

instance TooMany Goats where
  tooMany (Goats (x, y)) = x + y > 42



test = tooMany (Goats (42, 1))
