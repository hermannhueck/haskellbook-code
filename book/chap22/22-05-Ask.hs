-- 22-05-Ask.hs
--
-- 22.5 But uh, Reader, page 871
-- Exercise: Ask, page 873
--
{-# LANGUAGE InstanceSigs #-}

module Ask where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
  -- fmap f (Reader ra) = Reader $ (f . ra)

ask :: Reader a a
ask = Reader id
