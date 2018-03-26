-- 06-05c-Identity.hs
--
-- 6.5 Writing typeclass instances, page 169
-- Identity, page 177
--
module Identity where

data Identity a = Identity a deriving (Show)

{- constraint missing
instance Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
-}

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

isIdentical :: Eq a => a -> a -> Bool
isIdentical x y = Identity x == Identity y
