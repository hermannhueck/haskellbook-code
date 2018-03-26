-- 11-09-LogicGoats1a.hs
--
-- 11.9 newtype, page 406
-- Exercises: Logic Goats, page 410
--
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats1a where

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String). This will require adding a language

-- a) solution without newtype

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n,s) = n > 42



test = tooMany (43::Int, "goats")
