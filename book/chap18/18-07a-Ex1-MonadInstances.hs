-- 18-07a-MonadInstances.hs
--
-- 18.07 Chapter Exercises, page 788
-- Monad instances, page 788
--
-- Write Monod instance for the following types. Use the
-- QuickCheck Properties to validate their instances.
--
module MonadInstances where

import Control.Monad (join)
import Data.Monoid ((<>))
import Prelude hiding (Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----------------------------------------------------------------------------------------------
-- 1.
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

type NopeSSI = Nope (String, String, Int)

quickCheck_Nope :: IO ()
quickCheck_Nope = do
  putStrLn "\n----- Testing Nope a --------------"
  putStrLn "quickBatch (functor (undefined :: NopeSSI))"
  quickBatch (functor (undefined :: NopeSSI))
  putStrLn "\nquickBatch (applicative (undefined :: NopeSSI))"
  quickBatch (applicative (undefined :: NopeSSI))
  putStrLn "\nquickBatch (monad (undefined :: NopeSSI))"
  quickBatch (monad (undefined :: NopeSSI))

----------------------------------------------------------------------------------------------
-- 2.
data PhhhbbtttEither b a
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right x) = Right x
  fmap f (Left x) = Left $ f x

instance Applicative (PhhhbbtttEither b) where
  pure x = Left x
  Left f <*> rOrL = f <$> rOrL
  Right x <*> _ = Right x

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right x >>= _ = Right x
  Left x >>= f = f x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Left <$> arbitrary, Right <$> arbitrary]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

type PhhhbbtttEitherSSI
   = PhhhbbtttEither Int (String, String, Int)

quickCheck_PhhhbbtttEither :: IO ()
quickCheck_PhhhbbtttEither = do
  putStrLn "\n----- Testing PhhhbbtttEither b a --------------"
  putStrLn "quickBatch (functor (undefined :: PhhhbbtttEitherSSI))"
  quickBatch (functor (undefined :: PhhhbbtttEitherSSI))
  putStrLn "\nquickBatch (applicative (undefined :: PhhhbbtttEitherSSI))"
  quickBatch (applicative (undefined :: PhhhbbtttEitherSSI))
  putStrLn "\nquickBatch (monad (undefined :: PhhhbbtttEitherSSI))"
  quickBatch (monad (undefined :: PhhhbbtttEitherSSI))

----------------------------------------------------------------------------------------------
-- 3.
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> ident = fmap f ident

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

type IdentitySSI = Identity (String, String, Int)

quickCheck_Identity :: IO ()
quickCheck_Identity = do
  putStrLn "\n----- Testing Identity a --------------"
  putStrLn "quickBatch (functor (undefined :: IdentitySSI))"
  quickBatch (functor (undefined :: IdentitySSI))
  putStrLn "\nquickBatch (applicative (undefined :: IdentitySSI))"
  quickBatch (applicative (undefined :: IdentitySSI))
  putStrLn "\nquickBatch (monad (undefined :: IdentitySSI))"
  quickBatch (monad (undefined :: IdentitySSI))

----------------------------------------------------------------------------------------------
-- 4.
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  Nil `mappend` ys = ys
  (Cons x xs) `mappend` ys = Cons x (xs <> ys)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)
  -- xs >>= f = join $ f <$> xs  --- !!! this impl loops infinitly in the test

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency $ zip [1, 4] $ [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

type ListSSI = List (String, String, Int)

quickCheck_List :: IO ()
quickCheck_List = do
  putStrLn "\n----- Testing List a --------------"
  putStrLn "quickBatch (monoid (undefined :: ListSSI))"
  quickBatch (monoid (undefined :: ListSSI))
  putStrLn "quickBatch (functor (undefined :: ListSSI))"
  quickBatch (functor (undefined :: ListSSI))
  putStrLn "\nquickBatch (applicative (undefined :: ListSSI))"
  quickBatch (applicative (undefined :: ListSSI))
  putStrLn "\nquickBatch (monad (undefined :: ListSSI))"
  quickBatch (monad (undefined :: ListSSI))

----------------------------------------------------------------------------------------------
-- Invoking tests in main
main :: IO ()
main = do
  quickCheck_Nope
  quickCheck_PhhhbbtttEither
  quickCheck_Identity
  quickCheck_List
