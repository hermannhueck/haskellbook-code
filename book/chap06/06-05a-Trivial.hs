-- 06-05a-Trivial.hs
--
-- 6.5 Writing typeclass instances, page 169
-- Trivial, page 170
--
module Trivial where

data Trivial = Trivial'
  deriving (Show)

instance Eq Trivial where
  (==) Trivial' Trivial' = True
