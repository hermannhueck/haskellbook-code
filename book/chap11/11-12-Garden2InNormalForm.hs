-- 11-12-Garden2InNormalForm.hs
--
-- 11.12 Normal form, page 417
-- Exercises: How Does Your Garden Grow? page 420
--
module Garden2InNormalForm where

type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)
