-- 22-06b-ReadingComprehension.hs
--
-- 22.6 Functions have an Applicative too, page 873
-- Exercise: Reading Comprehension, page 877
--
{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

------------------------------------------------------------------------------------
-- 1.
-- Write liftA2 yourself. Think about it in terms of abstracting out
-- the difference of getDogR and getDogR' if that helps.

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2  :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

------------------------------------------------------------------------------------
-- 2. Write the function 'asks'.

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

------------------------------------------------------------------------------------
-- 3. Implement the Applicative for Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
  -- or:             = Reader $ (f . ra)

-- Some instructions and hints:
-- a) When writing the pure function for Reader, remember that what you're trying to construct
--    is a function that takes a value of type r, which you know nothing about, and return a
--    value of type a. Given that you're not really doing anything with r, there 's really one
--    thing you can do.
-- b) We got the definition of the apply function startet for you, we'll describe what you need
--    to do and you write the code. If you unpack the type of Reader's apply above, you get the
--    following:
--    <*> :: (r -> a -> b) -> (r -> a) -> (r -> b)
--    -- contrast this with the type of fmap
--    fmap :: (a -> b) -> (r -> a) -> (r -> b)
--    So what's the difference? The difference is that apply, unlike fmap, also takes an argument
--    of type r. Make it so.

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
  -- or:                       = Reader $ \r -> rab r $ ra r
  -- or:                       = Reader $ rab <*> ra

------------------------------------------------------------------------------------
-- tests in main
main :: IO ()
main = do
  let trigger = undefined :: [(Int, Int, [Int])]
  putStrLn "\nTesting myLiftA2 to behave as liftA2 does ..."
  quickCheck ((\x -> (myLiftA2 (+) (*2) (+10)) x == (myLiftA2 (+) (*2) (+10)) x) :: Integer -> Bool)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
