-- 21-12b-Ex2-SkiFree.hs
--
-- 21.12 Chapter Exercises, page 856
-- S, page 857
--
-- This may be difficult. To make it easier, we'll give you
-- the constraints and QuickCheck instances.
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module SkiFree where

import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a
  deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- instance ( Arbitrary (n a)
--          , CoArbitrary (n a)
--          , Arbitrary a
--          , CoArbitrary a
--          ) => Arbitrary (S n a) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     return $ S (x y) y

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

-- instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
--   (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap :: (a -> b) -> S n a -> S n b
  fmap f (S nx x) = S (fmap f nx) (f x)

instance Applicative n => Applicative (S n) where
  -- pure :: Applicative f => a -> f a
  pure :: a -> S n a
  pure x = S (pure x) x
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) :: S n (a -> b) -> S n a -> S n b
  S nf g <*> S nx y = S (nf <*> nx) (g y)

instance Foldable n => Foldable (S n) where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S nx x) = foldMap f nx <> f x
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr :: (a -> b -> b) -> b -> S n a -> b
  foldr f z (S nx x) = foldr f (f x z) nx

instance Traversable n => Traversable (S n) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> S n a -> f (S n b)  -- must allow InstanceSigs (see above)
  traverse f (S nx x) = S <$> traverse f nx <*> f x
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => S n a -> f (S n a)
  sequenceA (S nx x) = S <$> sequenceA nx <*> x

type ISS = (Int, String, String)

main :: IO () -- [S [] Int]
main = do
  putStr "\n----- Testing Traversable: S (n a) a --------------"
  quickBatch (functor (undefined :: S [] ISS))
  -- quickBatch (applicative (undefined :: S [] ISS))       -- test lasts very long
  quickBatch (traversable (undefined :: S [] ISS))
  putStrLn "\n----- Printing some samples of: S[] Int --------------"
  sample (arbitrary :: Gen (S [] Int))
  -- sample' (arbitrary :: Gen (S [] Int))
