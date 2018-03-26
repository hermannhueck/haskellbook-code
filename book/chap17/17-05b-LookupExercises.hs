-- 17-05b-LookupExercises.hs
--
-- 17.05 Applicative in use, page 696
-- Exercises: Lookup, page 702
--
-- In the following exercises you will need to use the following terms to make the expressions typecheck:
-- 1. pure
-- 2. (<$>)   -- or fmap
-- 3. (<*>)
--
module LookupExercises where

import Test.Hspec
import Control.Applicative
import Data.Monoid
import Data.List (elemIndex)

-- Make the following expressions typecheck:
--
-- 1.
added :: Maybe Integer
-- wrong: added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

added' :: Maybe Integer
-- wrong: added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added' = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
-- wrong: tupled = (,) y z
tupled = (,) <$> y <*> z

tupled' :: Maybe (Integer, Integer)
tupled' = liftA2 (,) y z

-- 3.
x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 2 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
-- wrong: maxed = max' x' y'
maxed = max' <$> x' <*> y'

maxed' :: Maybe Int
maxed' = liftA2 max' x' y'

-- 3.
xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
-- wrong: summed = sum $ (,) x'' y''
summed = (+) <$> x'' <*> y''              -- returns Just (sum of x'' and y'')

summed' :: Maybe Integer
summed' = sum <$> ((,) <$> x'' <*> y'')   -- returns Just (value of y'')

summed'' :: Maybe Integer
summed'' = sum <$> liftA2 (,) x'' y''     -- returns Just (value of y'')

-- tests with Hspec
main :: IO ()
main =
  hspec $ do
    describe "Testing Lookups ..." $ do
      it "added `shouldBe` Just 9" $ do
        added `shouldBe` Just 9
      it "added' `shouldBe` Just 9" $ do
        added' `shouldBe` Just 9
      it "tupled `shouldBe` Just (6, 5)" $ do
        tupled `shouldBe` Just (6, 5)
      it "tupled' `shouldBe` Just (6, 5)" $ do
        tupled' `shouldBe` Just (6, 5)
      it "maxed `shouldBe` Just 2" $ do
        maxed `shouldBe` Just 2
      it "maxed' `shouldBe` Just 2" $ do
        maxed' `shouldBe` Just 2
      it "summed `shouldBe` Just 11" $ do
        summed `shouldBe` Just 11
      it "summed' `shouldBe` Just 5" $ do
        summed' `shouldBe` Just 5
      it "summed'' `shouldBe` Just 5" $ do
        summed'' `shouldBe` Just 5
