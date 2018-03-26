-- 23-06-Moi.hs
--
-- 23.6 Write State yourself, page 903
-- L'Etat c'est moi, page 903
--
{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> (f $ fst $ g s, s)
  -- or:         = Moi $ \s -> ((f . fst . g) s, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> ((fst $ f s) (fst $ g s), s)

instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g = Moi $
    \s -> let (a, s') = f s
          in runMoi (g a) s'
  -- or:      = Moi $ \s -> (runMoi . g . fst . f) s (snd . f $ s)
