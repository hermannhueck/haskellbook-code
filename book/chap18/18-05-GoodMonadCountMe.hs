-- 18-05-GoodMonadCountMe.hs
--
-- 18.05 Monad laws, page 776
-- Bad Monads and their denizens, page 779
--
-- Invalid Monad (an d Applicative and Functor) made valid.
--
module GoodMonadCountMe where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer
          a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

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
