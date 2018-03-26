-- 21-12c-Ex3-TraversableTree.hs
--
-- 21.12 Chapter Exercises, page 856
-- TraversableTree, page 858
--
-- This might be hard. Write the following instances for Tree.
--
-- 1. for foldMap think of Functor but with some Monoid thrown in.
-- 2. for traverse think of Functor but with some Functor thrown in.
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module TraversableTree where

import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

instance Functor Tree where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node tl x tr) = Node (f <$> tl) (f x) (f <$> tr)

-- foldMap is a bit easier and looks more natural, but you can do foldr too for extra credit.
instance Foldable Tree where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node tlx x trx) = (foldMap f tlx) <> (f x) <> (foldMap f trx)
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node tlx x trx) = foldr f z' tlx     -- left tree with result of value and right tree
    where z' = f x z''                            -- compute value with result of right tree
          z'' = foldr f z trx                     -- compute right tree

instance Traversable Tree where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)  -- must allow InstanceSigs (see above)
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node tlx x trx) = Node <$> (traverse f tlx) <*> (f x) <*> (traverse f trx)
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA Empty = pure Empty
  sequenceA (Leaf x) = Leaf <$> x
  sequenceA (Node tlx x trx) = Node <$> sequenceA tlx <*> x <*> sequenceA trx

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    tl <- arbitrary
    x <- arbitrary
    tr <- arbitrary
    frequency
      $ zip [1, 1, 1]
      $ map return
        [Empty, Leaf x, Node tl x tr]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- Test with QuickCheck and Checkers
main :: IO ()
main = do
  let trigger = undefined :: Tree (Int, String, String)
  putStr "\n----- Testing Traversable: Tree a --------------"
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
