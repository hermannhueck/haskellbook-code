-- 20-04b-FoldMaybe.hs
--
-- 20.04 Demonstrating Foldable instances, page 828
-- Maybe, page 829
--
--
module FoldMaybe where

import Data.Monoid
import Data.Foldable

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x
  