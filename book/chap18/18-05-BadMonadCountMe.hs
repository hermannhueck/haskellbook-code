-- 18-05-BadMonadCountMe.hs
--
-- 18.05 Monad laws, page 776
-- Bad Monads and their denizens, page 779
--
-- We're going to write an invalid Monad (and Functor). You could pretend it's Identity
-- with an Integer thrown in which gets incremented on each fmap or bind.
--
module BadMonadCountMe where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer
          a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + 1) (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe _ b = f a
    in CountMe (n + 1) b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

type CountMeSSI = CountMe (String, String, Int)

main :: IO ()
main = do
  putStrLn "\n----- Testing CountMe a --------------"
  putStrLn "quickBatch (functor (undefined :: CountMeSSI))"
  quickBatch (functor (undefined :: CountMeSSI))
  putStrLn "\nquickBatch (applicative (undefined :: CountMeSSI))"
  quickBatch (applicative (undefined :: CountMeSSI))
  putStrLn "\nquickBatch (monad (undefined :: CountMeSSI))"
  quickBatch (monad (undefined :: CountMeSSI))
