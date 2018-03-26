-- 16-16-FlipFunctor.hs

-- 16.16 Functors are unique to a datatype, page 678
--


{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where
    
import Test.QuickCheck
import Test.Hspec


data Tuple a b = Tuple a b
                 deriving (Eq, Show)

newtype Flip f a b = Flip (f b a)
                     deriving (Eq, Show)

-- this works, goofy as it looks.
instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b


main :: IO ()
main = hspec $ do
    describe "Checking FlipFunctor ..." $ do
        it "fmap (+1) (Flip (Tuple 1 \"blah\")) `shouldBe` Flip (Tuple 2 \"blah\")" $ do
            fmap (+1) (Flip (Tuple 1 "blah")) `shouldBe` Flip (Tuple 2 "blah")
