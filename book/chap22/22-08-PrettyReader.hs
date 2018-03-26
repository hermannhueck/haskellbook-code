-- 22-08-PrettyReader.hs
--
-- 22.8 Reader Monad by itself is boring, page 882
--

{-# LANGUAGE NoImplicitPrelude #-}

module PrettyReader where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

const :: a -> b -> a
const a b = a

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \a -> f (g a)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) r) where
  pure = const
  f <*> a = \r -> f r (a r)

instance Monad ((->) r) where
  return = pure
  m >>= k = flip k <*> m

-- 22.9 You can change what comes below, but not above

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT (runReaderT m . f)