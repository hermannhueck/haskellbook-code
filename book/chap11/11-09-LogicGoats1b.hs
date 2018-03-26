-- 11-09-LogicGoats1b.hs
--
-- 11.9 newtype, page 406
-- Exercises: Logic Goats, page 410
--
module LogicGoats1b where

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String). This will require adding a language

-- b) solution with newtype

class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats (Int, String) deriving Show

instance TooMany Goats where
  tooMany (Goats (n, s)) = n > 42



test = tooMany (Goats (43, "goats"))
