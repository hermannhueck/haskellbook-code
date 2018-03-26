-- 17-08a-ZipListMonoid.hs
--
-- 17.08 ZipList Monoid, page 730
--
--
module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  -- mempty = falsy: ZipList []       -- !!! wrong: violates Monoid identity laws
  mempty = pure mempty                -- provides a ZipList containing an infinite List of empty Lists
  mappend = liftA2 mappend

-- !!! Arbitrary instances need not be defined
-- !!! Already defined in newer versions of Checker
--
-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary
--
-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

main :: IO ()
main = do
  putStrLn "\nTesting ZipList with Checkers ..."
  putStrLn "\nIdentity checks failed in the previous version (see definition of mempty)."
  putStrLn "quickBatch (monoid (undefined :: ZipList [Sum Int]))"
  quickBatch (monoid (undefined :: ZipList [Sum Int]))
  