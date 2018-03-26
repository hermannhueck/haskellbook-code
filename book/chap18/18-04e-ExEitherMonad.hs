-- 18-04e-ExEitherMonad.hs
--
-- 18.04 Examples of Monad use, page 759
-- Short exercise: Either Monad, page 776
--
-- Implement the Either monad.
--
module ExEitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)      = First x
  fmap f (Second y)     = Second $ f y

instance Applicative (Sum a) where
  pure = Second
  Second f <*> sum      = f <$> sum
  First x <*> _         = First x
  
instance Monad (Sum a) where
  return = pure
  First x >>= _         = First x
  Second x >>= f        = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

type SumSSI = Sum Int (String, String, Int)

main :: IO ()
main = do
  putStrLn "\n----- Testing Sum a b --------------"
  putStrLn "quickBatch (functor (undefined :: SumSSI))"
  quickBatch (functor (undefined :: SumSSI))
  putStrLn "\nquickBatch (applicative (undefined :: SumSSI))"
  quickBatch (applicative (undefined :: SumSSI))
  putStrLn "\nquickBatch (monad (undefined :: SumSSI))"
  quickBatch (monad (undefined :: SumSSI))
