-- 11-10-PityTheBool.hs
--
-- 11.10 Sum types, page 411
-- Exercises: Pity the Bool, page 412
--
{-# LANGUAGE NegativeLiterals #-}

module PityTheBool where

import Data.Int
  
-- Determine cardinalities

-- 1.
data BigSmall =
      Big Bool
    | Small Bool deriving (Eq, Show)
-- cardinality: 2 * 2 = 4


-- 2
data NumberOrBool =
      Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)
-- cardinality: 256 * 2 = 512

-- parentheses due to syntactic collision between (-) minus and the negate function
let myNumba = Numba (-128)
-- use pragma NegativeLiterals to avoid warning ...

-- bind (-128) to an intermediate variable
let n = (-128)
let myNumba2 = Numba n
