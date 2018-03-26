-- 16-12-Constant.hs
--
-- 16.12 A somewhat surprising functor, page 669
--


module Constant where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int


newtype Constant a b = Constant { getConstant :: a }        -- b is a phantom type
                       deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        x <- arbitrary
        return $ Constant x


main :: IO ()
main = hspec $ do
    describe "Checking Functor 'Const' manually ..." $ do
        it "const 1 1 `shouldBe` 1" $ do
            const 1 1 `shouldBe` 1
        it "const 1 2 `shouldBe` 1" $ do
            const 1 2 `shouldBe` 1
        it "const 1 3 `shouldBe` 1" $ do
            const 1 3 `shouldBe` 1
        it "const 1 \"blah\" `shouldBe` 1" $ do
            const 1 "blah" `shouldBe` 1
        it "Constant 2 `shouldBe` Constant 2" $ do
            (Constant 2) `shouldBe` Constant 2
        it "getConstant (Constant 2) `shouldBe` 2" $ do
            getConstant (Constant 2) `shouldBe` 2
        it "const 2 (getConstant (Constant 3)) `shouldBe` 2" $ do
            const 2 (getConstant (Constant 3)) `shouldBe` 2
        it "fmap (const 2) (Constant 3) `shouldBe` Constant 3" $ do
            fmap (const 2) (Constant 3) `shouldBe` Constant 3
        it "getConstant (fmap (const 2) (Constant 3)) `shouldBe` 3" $ do
            getConstant (fmap (const 2) (Constant 3)) `shouldBe` 3
        it "getConstant (fmap (const \"blah\") (Constant 3)) `shouldBe` 3" $ do
            getConstant (fmap (const "blah") (Constant 3)) `shouldBe` 3
        it "getConstant (id (Constant 3)) `shouldBe` 3" $ do
            getConstant (id (Constant 3)) `shouldBe` 3
        it "getConstant (fmap id (Constant 3)) `shouldBe` 3" $ do
            getConstant (fmap id (Constant 3)) `shouldBe` 3
        it "getConstant (fmap id (Constant 3)) `shouldBe` getConstant (id (Constant 3)) -- identity law" $ do
            getConstant (fmap id (Constant 3)) `shouldBe` getConstant (id (Constant 3))
        it "((fmap (const 3)) . (fmap (const 5))) (Constant \"WOOHOO\") `shouldBe` fmap ((const 3) . (const 5)) (Constant \"WOOHOO\") -- composition law" $ do
            ((fmap (const 3)) . (fmap (const 5))) (Constant "WOOHOO") `shouldBe` fmap ((const 3) . (const 5)) (Constant "WOOHOO")
        it "((fmap (const 3)) . (fmap (const 5))) (Constant \"Dogs rule\") `shouldBe` fmap ((const 3) . (const 5)) (Constant \"Dogs rule\") -- composition law" $ do
            ((fmap (const 3)) . (fmap (const 5))) (Constant "Dogs rule") `shouldBe` fmap ((const 3) . (const 5)) (Constant "Dogs rule")
    describe "\nQuickChecking Functor 'Const' ..." $ do
        it "Functor identity law holds." $ do
            property (functorIdentity :: Constant Int Int -> Bool)
        it "Functor identity law holds for functions: (+1) (*2)" $ do
            property ((functorCompose (+1) (*2)) :: Constant Int Int -> Bool)
        it "Functor identity law holds for arbitrary functions." $ do
            property (functorCompose' :: Constant Int Int -> IntToInt -> IntToInt -> Bool)
    