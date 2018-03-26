-- 11-12-Garden1NotNormalForm.hs
--
-- 11.12 Normal form, page 417
-- Exercises: How Does Your Garden Grow? page 420
--
module Garden1NotNormalForm where

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving (Show)

-- What is the sum of products normal form of Garden?
-- See gardenInNormalForm.hs
