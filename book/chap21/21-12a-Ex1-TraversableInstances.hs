-- 21-12a-Ex1-TraversableInstances.hs
--
-- 21.12 Chapter Exercises, page 856
-- Traversable instances, page 856
--
{-# LANGUAGE InstanceSigs #-}

module TraversableInstances where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

------------------------------------------------
-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity x = Identity $ f x

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity x) = f x
  foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Identity a -> f (Identity b)  -- must allow InstanceSigs (see above)
  traverse f (Identity x) = Identity <$> f x
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Identity (f a) -> f (Identity a)
  sequenceA (Identity fa) = Identity <$> fa

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

------------------------------------------------
-- Constant
newtype Constant c b = Constant { getConstant :: c }
  deriving (Eq, Ord, Show)

instance Functor (Constant c) where
  fmap :: (a -> b) -> Constant c a -> Constant c b
  fmap _ (Constant x) = Constant x

instance Monoid c => Applicative (Constant c) where
  pure :: a -> Constant c a
  pure _ = Constant mempty
  (<*>) :: Constant c (a -> b) -> Constant c a -> Constant c b
  Constant x <*> Constant x' = Constant $ x <> x'

instance Foldable (Constant c) where
  foldMap :: Monoid m => (a -> m) -> Constant c a -> m
  foldMap _ _ = mempty
  foldr :: (a -> b -> b) -> b -> Constant c a -> b
  foldr _ z _ = z

instance Traversable (Constant c) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Constant c a -> f (Constant c b)  -- must allow InstanceSigs (see above)
  traverse _ (Constant x) = pure $ Constant x
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Constant c (f a) -> f (Constant c a)
  sequenceA (Constant x) = pure $ Constant x

instance Arbitrary c => Arbitrary (Constant c b) where
  arbitrary = Constant <$> arbitrary

instance (Eq c, Eq b) => EqProp (Constant c b) where
  (=-=) = eq

------------------------------------------------
-- Maybe
data Optional a = Nada | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Yep
  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  Nada <*> _ = Nada
  Yep f <*> opt = fmap f opt

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x
  foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

instance Traversable Optional where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Optional a -> f (Optional b)  -- must allow InstanceSigs (see above)
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Optional (f a) -> f (Optional a)
  sequenceA Nada = pure Nada
  sequenceA (Yep fa) = Yep <$> fa

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

------------------------------------------------
-- List
data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Monoid (List a) where
  mempty :: List a
  mempty = Nil
  mappend :: List a -> List a -> List a
  Nil `mappend` ys = ys
  (Cons x xs) `mappend` ys = Cons x (xs <> ys)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil
  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)
  
instance Foldable List where
  foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Traversable List where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> List a -> f (List b)  -- must allow InstanceSigs (see above)
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons fa fas) = Cons <$> fa <*> sequenceA fas

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency $ zip [1, 8] [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

------------------------------------------------
-- Three
data Three a b c = Three a b c
  deriving (Eq, Ord, Show)

instance (Monoid fst, Monoid snd) => Applicative (Three fst snd) where
  pure :: a -> Three fst snd a
  pure x = Three mempty mempty x
  (<*>) :: Three fst snd (a -> b) -> Three fst snd a -> Three fst snd b
  Three r s f <*> Three v w x = Three (r <> v) (s <> w) (f x)

instance Functor (Three fst snd) where
  fmap :: (a -> b) -> Three fst snd a -> Three fst snd b
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three fst snd) where
  foldMap :: Monoid m => (a -> m) -> Three fst snd a -> m
  foldMap f (Three _ _ x) = f x
  foldr :: (a -> b -> b) -> b -> Three fst snd a -> b
  foldr f z (Three _ _ x) = f x z

instance Traversable (Three fst snd) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Three fst snd a -> f (Three fst snd b)  -- must allow InstanceSigs (see above)
  traverse f (Three x y a) = (Three x y) <$> f a
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Three fst snd (f a) -> f (Three fst snd a)
  sequenceA (Three x y fa) = (Three x y) <$> fa

instance (Arbitrary fst, Arbitrary snd, Arbitrary thrd) => Arbitrary (Three fst snd thrd) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq fst, Eq snd, Eq thrd) => EqProp (Three fst snd thrd) where
  (=-=) = eq

------------------------------------------------
-- Pair
data Pair fst snd = Pair fst snd
  deriving (Eq, Ord, Show)

instance Functor (Pair fst) where
  fmap :: (a -> b) -> Pair fst a -> Pair fst b
  fmap f (Pair x y) = Pair x (f y)

instance Monoid fst => Applicative (Pair fst) where
  pure :: a -> Pair fst a
  pure x = Pair mempty x
  (<*>) :: Pair fst (a -> b) -> Pair fst a -> Pair fst b
  Pair u f <*> Pair v x = Pair (u <> v) (f x)

instance Foldable (Pair fst) where
  foldMap :: Monoid m => (a -> m) -> Pair fst a -> m
  foldMap f (Pair _ y) = f y
  foldr :: (a -> b -> b) -> b -> Pair fst a -> b
  foldr f z (Pair _ y) = f y z

instance Traversable (Pair fst) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Pair fst a -> f (Pair fst b)  -- must allow InstanceSigs (see above)
  traverse f (Pair x a) = (Pair x) <$> f a
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Pair fst (f a) -> f (Pair fst a)
  sequenceA (Pair x fa) = (Pair x) <$> fa

instance (Arbitrary fst, Arbitrary snd) => Arbitrary (Pair fst snd) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq fst, Eq snd) => EqProp (Pair fst snd) where
  (=-=) = eq

------------------------------------------------
-- Big
data Big fst snd = Big fst snd snd
  deriving (Eq, Ord, Show)

instance Functor (Big fst) where
  fmap :: (a -> b) -> Big fst a -> Big fst b
  fmap f (Big x y z) = Big x (f y) (f z)

instance Monoid fst => Applicative (Big fst) where
  pure :: a -> Big fst a
  pure x = Big mempty x x
  (<*>) :: Big fst (a -> b) -> Big fst a -> Big fst b
  Big u f g <*> Big v x y = Big (u <> v) (f x) (f y)

instance Foldable (Big fst) where
  foldMap :: Monoid m => (a -> m) -> Big fst a -> m
  foldMap f (Big _ x y) = f x <> f y
  foldr :: (a -> b -> b) -> b -> Big fst a -> b
  foldr f z (Big _ x y) = f x $ f y z

instance Traversable (Big fst) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Big fst a -> f (Big fst b)  -- must allow InstanceSigs (see above)
  traverse f (Big x a1 a2) = (Big x) <$> f a1 <*> f a2
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Big fst (f a) -> f (Big fst a)
  sequenceA (Big x fa1 fa2) = (Big x) <$> fa1 <*> fa2

instance (Arbitrary fst, Arbitrary snd) => Arbitrary (Big fst snd) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq fst, Eq snd) => EqProp (Big fst snd) where
  (=-=) = eq

------------------------------------------------
-- Bigger
data Bigger fst snd = Bigger fst snd snd snd
  deriving (Eq, Ord, Show)

instance Functor (Bigger fst) where
  fmap :: (a -> b) -> Bigger fst a -> Bigger fst b
  fmap f (Bigger x y z z') = Bigger x (f y) (f z) (f z')

instance Monoid fst => Applicative (Bigger fst) where
  pure :: a -> Bigger fst a
  pure x = Bigger mempty x x x
  (<*>) :: Bigger fst (a -> b) -> Bigger fst a -> Bigger fst b
  Bigger u f g h <*> Bigger v x y z = Bigger (u <> v) (f x) (f y) (f z)

instance Foldable (Bigger fst) where
  foldMap :: Monoid m => (a -> m) -> Bigger fst a -> m
  foldMap f (Bigger _ x y z) = f x <> f y <> f z
  foldr :: (a -> b -> b) -> b -> Bigger fst a -> b
  foldr f zero (Bigger _ x y z) = f x $ f y $ f z zero

instance Traversable (Bigger fst) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse :: (Applicative f) => (a -> f b) -> Bigger fst a -> f (Bigger fst b)  -- must allow InstanceSigs (see above)
  traverse f (Bigger x a1 a2 a3) = (Bigger x) <$> f a1 <*> f a2 <*> f a3
  -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
  sequenceA :: Applicative f => Bigger fst (f a) -> f (Bigger fst a)
  sequenceA (Bigger x fa1 fa2 fa3) = (Bigger x) <$> fa1 <*> fa2 <*> fa3

instance (Arbitrary fst, Arbitrary snd) => Arbitrary (Bigger fst snd) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq fst, Eq snd) => EqProp (Bigger fst snd) where
  (=-=) = eq

------------------------------------------------
-- Testing with QuickCheck and Checkers
type ISS = (Int, String, String)

main :: IO ()
main = do
  putStr "\n----- Testing Traversable: Identity a --------------"
  quickBatch (traversable (undefined :: Identity ISS))
  putStr "\n----- Testing Traversable: Constant a b --------------"
  quickBatch (traversable (undefined :: Constant Int ISS))
  putStr "\n----- Testing Traversable: Optional a --------------"
  quickBatch (traversable (undefined :: Optional ISS))
  putStr "\n----- Testing Traversable: List a --------------"
  quickBatch (traversable (undefined :: List ISS))
  putStr "\n----- Testing Traversable: Three fst snd thrd --------------"
  quickBatch (traversable (undefined :: Three Int Int ISS))
  putStr "\n----- Testing Traversable: Pair fst snd --------------"
  quickBatch (traversable (undefined :: Pair Int ISS))
  putStr "\n----- Testing Traversable: Big fst snd --------------"
  quickBatch (traversable (undefined :: Big Int ISS))
  putStr "\n----- Testing Traversable: Bigger fst snd --------------"
  quickBatch (traversable (undefined :: Bigger Int ISS))
