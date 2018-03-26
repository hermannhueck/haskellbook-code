-- 20-06-Exercises.hs
--
-- 20.06 Chapter Exercises, page 836
--
-- Write Foldable instances for the following datatypes.
--
module Exercises where

import Data.Semigroup hiding ((<>))
import Data.Monoid
import Data.Foldable

-----------------------------------------------------------------
-- 1.
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
  foldMap f (Constant x) = f x

-----------------------------------------------------------------
-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two x y) = f y z
  foldMap f (Two x y) = f y
  
-----------------------------------------------------------------
-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three i j k) = f k z
  foldMap f (Three i j k) = f k

-----------------------------------------------------------------
-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' i j k) = f j $ f k z
  foldMap f (Three' i j k) = f j <> f k

-----------------------------------------------------------------
-- 5.
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' i j k l) = f j $ f k $ f l z
  foldMap f (Four' i j k l) = f j <> f k <> f l

-----------------------------------------------------------------
-- 6. Thinking cap time. Write a filter function for Foldable types using foldMap.
filterF :: ( Applicative f , Foldable t , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

-- solution with foldr
filterF' :: ( Applicative f , Foldable t , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF' f = foldr (\x y -> if f x then pure x <> y else y) mempty
