-- 17-05d-ConstantInstance.hs
--
-- 17.05 Applicative in use, page 696
-- Exercise: Constant Instance, page 706
--
-- Write an Applicative instance for Identity.
--
module ConstantInstance where

import Test.Hspec
import Data.Monoid

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant x' = Constant $ x <> x'

-- tests with Hspec
main :: IO ()
main =
  hspec $ do
    describe "Testing Applicative instance for Constant ..." $ do
      it "(+1) <$> Constant 5 `shouldBe` Constant 5" $ do
        (+1) <$> Constant 5 `shouldBe` Constant 5
      it "pure 5 `shouldBe` Constant ()" $ do
        pure 5 `shouldBe` Constant ()
      it "Constant [3] <*> Constant [5] `shouldBe` Constant [3, 5]   -- List Monoid" $ do
        Constant [3] <*> Constant [5] `shouldBe` Constant [3, 5]
      it "Constant (Sum 3) <*> Constant (Sum 5) `shouldBe` Constant (Sum 8)   -- Sum Monoid" $ do
        Constant (Sum 3) <*> Constant (Sum 5) `shouldBe` Constant (Sum 8)
      it "Constant (Product 3) <*> Constant (Product 5) `shouldBe` Constant (Product 15)   -- Product Monoid" $ do
        Constant (Product 3) <*> Constant (Product 5) `shouldBe` Constant (Product 15)

-- Example page 706:
f = Constant (Sum 1)
g = Constant (Sum 2)
h = f <*> g