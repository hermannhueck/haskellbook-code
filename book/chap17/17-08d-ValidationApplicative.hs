-- 17-08c-ValidationApplicative.hs
--
-- 17.08 ZipList Monoid, page 730
-- Variations on Either, page 740
--
--
module ValidationApplicative where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Error =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)
type Errors = [Error]

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e)          = Failure e
  fmap f (Success v)          = Success $ f v

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success v     = Success $ f v
  Failure e <*> Failure e'    = Failure $ e `mappend` e'
  _ <*> Failure e             = Failure e
  Failure e <*> _             = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq
  
type ValidationOfTriples = Validation [String] (String, String, Int)

-- test with main
main :: IO ()
main = do
  hspec $ do
    describe "Simple Validation test with Hspec ..." $ do
      it "Success (+1) <*> Success 1 `shouldBe` Success 2" $ do
        Success (+1) <*> Success 1 `shouldBe` (Success 2 :: Validation Errors Integer)
      it "Success (+1) <*> Failure [StackOverflow] `shouldBe` Failure [StackOverflow]" $ do
        Success (+1) <*> Failure [StackOverflow] `shouldBe` (Failure [StackOverflow] :: Validation Errors Integer)
      it "Failure [StackOverflow] <*> Success 1 `shouldBe` Failure [StackOverflow]" $ do
        Failure [StackOverflow] <*> Success 1 `shouldBe` (Failure [StackOverflow] :: Validation Errors Integer)
      it "Failure [StackOverflow] <*> Success (+1) `shouldBe` Failure [StackOverflow]" $ do
        Failure [StackOverflow] <*> Success (+1) `shouldBe` (Failure [StackOverflow] :: Validation Errors Integer)
      it "Failure [MooglesChewedWires] <*> Failure [StackOverflow] `shouldBe` Failure [MooglesChewedWires, StackOverflow]" $ do
        Failure [MooglesChewedWires] <*> Failure [StackOverflow] `shouldBe` (Failure [MooglesChewedWires, StackOverflow] :: Validation Errors Integer)
  putStrLn "\nTesting List' with Checkers ..."
  putStrLn "\nquickBatch (functor (undefined :: ValidationOfTriples))"
  quickBatch $ functor (undefined :: ValidationOfTriples)
  putStrLn "\nquickBatch (applicative (undefined :: ValidationOfTriples))"
  quickBatch $ applicative (undefined :: ValidationOfTriples)
  putStrLn ""
