-- 22-07b-ReaderMonad.hs
--
-- 22.7 The Monad of functions, page 878
-- Exercise: Reader Monad
--
{-# LANGUAGE InstanceSigs #-}

module ReaderMonad where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
  -- or:             = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra
  -- or:                       = Reader $ \r -> rab r $ ra r
  -- or:                       = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r
  -- or:              = Reader $ \r -> (runReader . aRb . ra) r r
  -- or:              = Reader $ \r -> (runReader <$> aRb <$> ra) r r
  -- or:              = join $ Reader $ \r -> aRb (ra r)

------------------------------------------------------------------------------------
-- tests in main
main :: IO ()
main = do
  let trigger = undefined :: [(Int, Int, [Int])]
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
