-- 17-05c-IdentityInstance.hs
--
-- 17.05 Applicative in use, page 696
-- Exercise: Identy Instance, page 704
--
-- Write an Applicative instance for Identity.
--
module IdentityInstance where

import Test.Hspec

newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> id = f <$> id   -- same as
  -- (Identity f) <*> (Identity x) = Identity $ f x

-- tests with Hspec
main :: IO ()
main =
  hspec $ do
    describe "Testing Applicative instance for Identity ..." $ do
      it "(+1) <$> Identity 5 `shouldBe` Identity 6" $ do
        (+1) <$> Identity 5 `shouldBe` Identity 6
      it "pure 5 `shouldBe` Identity 5" $ do
        pure 5 `shouldBe` Identity 5
      it "Identity (+1) <*> Identity 5 `shouldBe` Identity 5" $ do
        Identity (+1) <*> Identity 5 `shouldBe` Identity 6
