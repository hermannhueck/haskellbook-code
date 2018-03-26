-- 15-10a-OptionalMonoid.hs
--
-- 15.10 Reusing algebras by asking for algebras, page 596
-- Exercise: Optional Monoid, page 597
--
module OptionalMonoid where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

-- Write the Monoid instance for our Maybe type renamed to Optional.

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only $ x <> y
  mappend opt Nada = opt
  mappend Nada opt = opt

main :: IO ()
main =
  hspec $ do
    describe "Testing Optional ..." $ do
      it "Only (Sum 1) <> Only (Sum 1) `shouldBe` Only (Sum 2)" $ do
        Only (Sum 1) <> Only (Sum 1) `shouldBe` Only (Sum 2)
      it "Only (Product 4) <> Only (Product 2) `shouldBe` Only (Product 8)" $ do
        Only (Product 4) <> Only (Product 2) `shouldBe` Only (Product 8)
      it "Only (Sum 1) <> Nada `shouldBe` Only (Sum 1)" $ do
        Only (Sum 1) <> Nada `shouldBe` Only (Sum 1)
      it "Nada <> Only (Sum 1) `shouldBe` Only (Sum 1)" $ do
        Nada <> Only (Sum 1) `shouldBe` Only (Sum 1)
      it "Only [1] <> Nada `shouldBe` Only [1]" $ do
        Only [1] <> Nada `shouldBe` Only [1]
      it "Only [1] <> Only [2,3] `shouldBe` Only [1,2,3]" $ do
        Only [1] <> Only [2, 3] `shouldBe` Only [1, 2, 3]
      it "Nada <> Nada `shouldBe` (Nada :: Optional (Sum Integer))" $ -- in hspec the type must be specified
       do
        Nada <> Nada `shouldBe` (Nada :: Optional (Sum Integer)) -- hspec must know which type of Optional to use
      it "Nada <> Nada `shouldBe` (Nada :: Optional [Integer])" $ do
        Nada <> Nada `shouldBe` (Nada :: Optional [Integer])
