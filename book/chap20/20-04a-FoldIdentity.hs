-- 20-04a-FoldIdentity.hs
--
-- 20.04 Demonstrating Foldable instances, page 828
-- Identity, page 829
--
--
module FoldIdentity where

import Data.Foldable
import Data.Monoid

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
