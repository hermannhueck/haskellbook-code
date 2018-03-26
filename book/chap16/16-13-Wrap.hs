-- 16-13-Wrap.hs

-- 16.13 More structure, more functors, page 672
--


module Wrap where

import Test.QuickCheck
import Test.Hspec


data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

{- DOES NOT WORK !!!
instance Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (f fa)
-}


{- DOES NOT WORK !!!
instance Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)
-}

-- Require f to be a Functor !!!
instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)


main :: IO ()
main = hspec $ do
    describe "Checking Functor 'Const' manually ..." $ do
        it "fmap (+1) (Wrap [1, 2, 3]) `shouldBe` Wrap [2, 3, 4]" $ do
            fmap (+1) (Wrap [1, 2, 3]) `shouldBe` Wrap [2, 3, 4]
        it "fmap (+1) (Wrap (Just 1)) `shouldBe` Wrap (Just 2)" $ do
            fmap (+1) (Wrap (Just 1)) `shouldBe` Wrap (Just 2)
        it "fmap (+1) (Wrap (Right 1)) `shouldBe` (Wrap (Right 2) :: Wrap (Either String) Int)" $ do
            fmap (+1) (Wrap (Right 1)) `shouldBe` (Wrap (Right 2) :: Wrap (Either String) Int)
        it "fmap (+1) (Wrap (Left \"Error\")) `shouldBe` (Wrap (Left \"Error\") :: Wrap (Either String) Int)" $ do
            fmap (+1) (Wrap (Left "Error")) `shouldBe` (Wrap (Left "Error") :: Wrap (Either String) Int)
