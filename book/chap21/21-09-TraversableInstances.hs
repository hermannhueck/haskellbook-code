-- 21-09-TraversableInstances.hs
--
-- 21.9 Traversable instances, page 852
--
{-# LANGUAGE InstanceSigs #-}

module TraversableInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

------------------------------------------------
-- Either'
data Either' a b = Left' a | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' e) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Either' l a -> Either' l b
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  -- pure :: Applicative f => a -> f a
  pure :: a -> Either' l a
  pure = Right'
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) :: Either' l (a -> b) -> Either' l a -> Either' l b
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' e) where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> Either' l a -> m
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr :: (a -> b -> b) -> b -> Either' l a -> b
  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' left) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Either' left a -> f (Either' left b)  -- must allow InstanceSigs (see above)
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Either' l (f a) -> f (Either' l a)
  sequenceA (Left' l) = pure $ Left' l
  sequenceA (Right' fa) = Right' <$> fa

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b) where
    arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Either' a b) where
    (=-=) = eq

------------------------------------------------
-- Tuple
data Tuple fst snd = Tuple fst snd
  deriving (Eq, Ord, Show)

instance Functor (Tuple fst) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Tuple fst a -> Tuple fst b
  fmap f (Tuple x y) = Tuple x (f y)

instance Monoid fst => Applicative (Tuple fst) where
  -- pure :: Applicative f => a -> f a
  pure :: a -> Tuple fst a
  pure x = Tuple mempty x
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) :: Tuple fst (a -> b) -> Tuple fst a -> Tuple fst b
  Tuple u f <*> Tuple v x = Tuple (u `mappend` v) (f x)

instance Foldable (Tuple fst) where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  foldMap :: Monoid m => (a -> m) -> Tuple fst a -> m
  foldMap f (Tuple _ y) = f y
  -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  foldr :: (a -> b -> b) -> b -> Tuple fst a -> b
  foldr f z (Tuple _ y) = f y z

instance Traversable (Tuple fst) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Tuple fst a -> f (Tuple fst b)
  traverse f (Tuple x y) = (Tuple x) <$> f y
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Tuple fst (f a) -> f (Tuple fst a)
  sequenceA (Tuple x fy) = (Tuple x) <$> fy

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
    arbitrary = Tuple <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Tuple a b) where
    (=-=) = eq

------------------------------------------------
-- Testing with QuickCheck and Checkers
type ISS = (Int, String, String)

main :: IO ()
main = do
  putStr "\n----- Testing Traversable: Either' left right --------------"
  quickBatch (traversable (undefined :: Either' Int ISS))
  putStr "\n----- Testing Traversable: Tuple fst snd --------------"
  quickBatch (traversable (undefined :: Tuple Int ISS))
  