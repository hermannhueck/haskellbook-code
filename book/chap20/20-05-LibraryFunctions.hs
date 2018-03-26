-- 20-05-LibraryFunctions.hs
--
-- 20.05 Some basic derived operations, page 831
-- Exercises: Library Functions, page 835
--
-- Implement the functions in terms of foldMap or foldr from Foldable,
-- and then try them out with multiple types that have foldable instances.
--
module LibraryFunctions where

import Data.Semigroup hiding ((<>))
import Data.Monoid
import Data.Foldable
import Test.Hspec
import Test.QuickCheck

-----------------------------------------------------------------
-- 1.
mySum1 :: (Num a, Foldable t) => t a -> a
mySum1 = foldr (+) 0

mySum2 :: (Num a, Foldable t) => t a -> a
mySum2 = getSum . foldMap Sum

test_sum :: IO ()
test_sum = hspec $ do
  describe "----- Test my impl of 'sum' ----------" $ do
    it "mySum1 (implemented with foldr  ) should behave like Foldable.sum (test with [Integer])." $ do
      property ((\xs -> mySum1 xs == sum xs) :: [Integer] -> Bool)
    it "mySum1 (implemented with foldr  ) should behave like Foldable.sum (test with Maybe Integer)." $ do
      property ((\xs -> mySum1 xs == sum xs) :: Maybe Integer -> Bool)
    it "mySum2 (implemented with foldMap) should behave like Foldable.sum (test with [Integer])." $ do
      property ((\xs -> mySum2 xs == sum xs) :: [Integer] -> Bool)
    it "mySum2 (implemented with foldMap) should behave like Foldable.sum (test with Maybe Integer)." $ do
      property ((\xs -> mySum2 xs == sum xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 2.
myProduct1 :: (Num a, Foldable t) => t a -> a
myProduct1 = foldr (*) 1

myProduct2 :: (Num a, Foldable t) => t a -> a
myProduct2 = getProduct . foldMap Product

test_product :: IO ()
test_product = hspec $ do
  describe "----- Test my impl of 'product' ----------" $ do
    it "myProduct1 (implemented with foldr  ) should behave like Foldable.product (test with [Integer])." $ do
      property ((\xs -> myProduct1 xs == product xs) :: [Integer] -> Bool)
    it "myProduct1 (implemented with foldr  ) should behave like Foldable.product (test with Maybe Integer)." $ do
      property ((\xs -> myProduct1 xs == product xs) :: Maybe Integer -> Bool)
    it "myProduct2 (implemented with foldMap) should behave like Foldable.product (test with [Integer])." $ do
      property ((\xs -> myProduct2 xs == product xs) :: [Integer] -> Bool)
    it "myProduct2 (implemented with foldMap) should behave like Foldable.product (test with Maybe Integer)." $ do
      property ((\xs -> myProduct2 xs == product xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 3.
myElem1 :: (Eq a, Foldable t) => a -> t a -> Bool
myElem1 x = foldr (\a acc -> a == x || acc) False

myElem2 :: (Eq a, Foldable t) => a -> t a -> Bool
myElem2 x = getAny . foldMap (\a -> Any (a == x))

test_elem :: IO ()
test_elem = hspec $ do
  describe "----- Test my impl of 'elem' ----------" $ do
    it "myElem1 (implemented with foldr  ) should behave like Foldable.elem (test with [Integer])." $ do
      property ((\x xs -> myElem1 x xs == elem x xs) :: Integer -> [Integer] -> Bool)
    it "myElem1 (implemented with foldr  ) should behave like Foldable.elem (test with Maybe Integer)." $ do
      property ((\x xs -> myElem1 x xs == elem x xs) :: Integer -> Maybe Integer -> Bool)
    it "myElem2 (implemented with foldMap) should behave like Foldable.elem (test with [Integer])." $ do
      property ((\x xs -> myElem2 x xs == elem x xs) :: Integer -> [Integer] -> Bool)
    it "myElem2 (implemented with foldMap) should behave like Foldable.elem (test with Maybe Integer)." $ do
      property ((\x xs -> myElem2 x xs == elem x xs) :: Integer -> Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 4.
-- 'myMinimum1' and 'myMinimum2' have other type signature than minimum from Prelude
-- minimum :: (Ord a, Foldable t) => t a -> a
--
myMinimum1 :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum1 = foldr minOf Nothing
  where
    minOf x Nothing = Just x
    minOf x (Just acc) = Just $ min x acc

myMinimum2 :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum2 = fmap getMin . getOption . foldMap (Option . Just . Min)

test_minimum :: IO ()
test_minimum = hspec $ do
  describe "----- Test my impl of 'minimum' ----------" $ do
    it "myMinimum1 (implemented with foldr  ) should behave like Foldable.minimum (test with [Integer])." $ do
      property ((\xs -> if null xs then myMinimum1 xs == Nothing else myMinimum1 xs == Just (minimum xs)) :: [Integer] -> Bool)
    it "myMinimum1 (implemented with foldr  ) should behave like Foldable.minimum (test with Maybe Integer)." $ do
      property ((\xs -> if null xs then myMinimum1 xs == Nothing else myMinimum1 xs == Just (minimum xs)) :: Maybe Integer -> Bool)
    it "myMinimum2 (implemented with foldMap) should behave like Foldable.minimum (test with [Integer])." $ do
      property ((\xs -> if null xs then myMinimum2 xs == Nothing else myMinimum2 xs == Just (minimum xs)) :: [Integer] -> Bool)
    it "myMinimum2 (implemented with foldMap) should behave like Foldable.minimum (test with Maybe Integer)." $ do
      property ((\xs -> if null xs then myMinimum2 xs == Nothing else myMinimum2 xs == Just (minimum xs)) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 5.
-- 'myMaximum1' and 'myMaximum2' have other type signature than 'maximum' from Prelude
-- maximum :: (Ord a, Foldable t) => t a -> a
--
myMaximum1 :: (Ord a, Foldable t) => t a -> Maybe a
myMaximum1 = foldr maxOf Nothing
  where
    maxOf x Nothing = Just x
    maxOf x (Just acc) = Just $ max x acc

myMaximum2 :: (Ord a, Foldable t) => t a -> Maybe a
myMaximum2 = fmap getMax . getOption . foldMap (Option . Just . Max)

test_maximum :: IO ()
test_maximum = hspec $ do
  describe "----- Test my impl of 'maximum' ----------" $ do
    it "myMaximum1 (implemented with foldr  ) should behave like Foldable.maximum (test with [Integer])." $ do
      property ((\xs -> if null xs then myMaximum1 xs == Nothing else myMaximum1 xs == Just (maximum xs)) :: [Integer] -> Bool)
    it "myMaximum1 (implemented with foldr  ) should behave like Foldable.maximum (test with Maybe Integer)." $ do
      property ((\xs -> if null xs then myMaximum1 xs == Nothing else myMaximum1 xs == Just (maximum xs)) :: Maybe Integer -> Bool)
    it "myMaximum2 (implemented with foldMap) should behave like Foldable.maximum (test with [Integer])." $ do
      property ((\xs -> if null xs then myMaximum2 xs == Nothing else myMaximum2 xs == Just (maximum xs)) :: [Integer] -> Bool)
    it "myMaximum2 (implemented with foldMap) should behave like Foldable.maximum (test with Maybe Integer)." $ do
      property ((\xs -> if null xs then myMaximum2 xs == Nothing else myMaximum2 xs == Just (maximum xs)) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 6.
myNull1 :: Foldable t => t a -> Bool
myNull1 = foldr (\a b -> False) True

myNull2 :: Foldable t => t a -> Bool
myNull2 = getAll . foldMap (\a -> All False)

test_null :: IO ()
test_null = hspec $ do
  describe "----- Test my impl of 'null' ----------" $ do
    it "myNull1 (implemented with foldr  ) should behave like Foldable.null (test with [Integer])." $ do
      property ((\xs -> myNull1 xs == null xs) :: [Integer] -> Bool)
    it "myNull1 (implemented with foldr  ) should behave like Foldable.null (test with Maybe Integer)." $ do
      property ((\xs -> myNull1 xs == null xs) :: Maybe Integer -> Bool)
    it "myNull2 (implemented with foldMap) should behave like Foldable.null (test with [Integer])." $ do
      property ((\xs -> myNull2 xs == null xs) :: [Integer] -> Bool)
    it "myNull2 (implemented with foldMap) should behave like Foldable.null (test with Maybe Integer)." $ do
      property ((\xs -> myNull2 xs == null xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 7.
myLength1 :: Foldable t => t a -> Int
myLength1 = foldr (\a b -> 1 + b) 0

myLength2 :: Foldable t => t a -> Int
myLength2 = getSum . foldMap (\a -> Sum 1)

test_length :: IO ()
test_length = hspec $ do
  describe "----- Test my impl of 'length' ----------" $ do
    it "myLength1 (implemented with foldr  ) should behave like Foldable.length (test with [Integer])." $ do
      property ((\xs -> myLength1 xs == length xs) :: [Integer] -> Bool)
    it "myLength1 (implemented with foldr  ) should behave like Foldable.length (test with Maybe Integer)." $ do
      property ((\xs -> myLength1 xs == length xs) :: Maybe Integer -> Bool)
    it "myLength2 (implemented with foldMap) should behave like Foldable.length (test with [Integer])." $ do
      property ((\xs -> myLength2 xs == length xs) :: [Integer] -> Bool)
    it "myLength2 (implemented with foldMap) should behave like Foldable.length (test with Maybe Integer)." $ do
      property ((\xs -> myLength2 xs == length xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 8.
myToList1 :: Foldable t => t a -> [a]
myToList1 = foldr (:) []

myToList2 :: Foldable t => t a -> [a]
myToList2 = foldMap (\a -> [a])

test_toList :: IO ()
test_toList = hspec $ do
  describe "----- Test my impl of 'toList' ----------" $ do
    it "myToList1 (implemented with foldr  ) should behave like Foldable.toList (test with [Integer])." $ do
      property ((\xs -> myToList1 xs == toList xs) :: [Integer] -> Bool)
    it "myToList1 (implemented with foldr  ) should behave like Foldable.toList (test with Maybe Integer)." $ do
      property ((\xs -> myToList1 xs == toList xs) :: Maybe Integer -> Bool)
    it "myToList2 (implemented with foldMap) should behave like Foldable.toList (test with [Integer])." $ do
      property ((\xs -> myToList2 xs == toList xs) :: [Integer] -> Bool)
    it "myToList2 (implemented with foldMap) should behave like Foldable.toList (test with Maybe Integer)." $ do
      property ((\xs -> myToList2 xs == toList xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 9.
myFold1 :: (Monoid m, Foldable t) => t m -> m
myFold1 = foldMap id

myFold2 :: (Monoid m, Foldable t) => t m -> m
myFold2 = foldr (\a b -> a <> b) mempty

test_fold :: IO ()
test_fold = hspec $ do
  describe "----- Test my impl of 'fold' ----------" $ do
    it "myFold1 (implemented with foldr  ) should behave like Foldable.fold (test with [Sum Integer])." $ do
      property ((\xs -> myFold1 xs == fold xs) :: [Sum Integer] -> Bool)
    it "myFold1 (implemented with foldr  ) should behave like Foldable.fold (test with Maybe (Sum Integer))." $ do
      property ((\xs -> myFold1 xs == fold xs) :: Maybe (Sum Integer) -> Bool)
    it "myFold2 (implemented with foldMap) should behave like Foldable.fold (test with [Sum Integer])." $ do
      property ((\xs -> myFold2 xs == fold xs) :: [Sum Integer] -> Bool)
    it "myFold2 (implemented with foldMap) should behave like Foldable.fold (test with Maybe (Sum Integer))." $ do
      property ((\xs -> myFold2 xs == fold xs) :: Maybe (Sum Integer) -> Bool)

-----------------------------------------------------------------
-- 10. impl foldMap in terms of foldr
myFoldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
myFoldMap f = foldr (\a b -> f a <> b) mempty

test_foldMap :: IO ()
test_foldMap = hspec $ do
  describe "----- Test my impl of 'foldMap' ----------" $ do
    it "myFoldMap (implemented with foldr  ) should behave like Foldable.foldMap (test with [Integer])." $ do
      property ((\xs -> myFoldMap Sum xs == foldMap Sum xs) :: [Integer] -> Bool)
    it "myFoldMap (implemented with foldr  ) should behave like Foldable.foldMap (test with Maybe Integer)." $ do
      property ((\xs -> myFoldMap Sum xs == foldMap Sum xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- 11. impl foldr in terms of foldMap (taken from the docs)
-- see: https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Foldable.html#t:Foldable
--
-- uses the Endo Monoid from Data.Monoid
{-
newtype Endo a = Endo { appEndo :: a -> a }
instance Monoid (Endo a) where
    mempty = Endo id
    (Endo f) `mappend` (Endo g) = Endo (f . g)
-}
myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z t = appEndo (foldMap (Endo . f) t ) z

test_foldr :: IO ()
test_foldr = hspec $ do
  describe "----- Test my impl of 'foldr' ----------" $ do
    it "myFoldr (implemented with foldMap) should behave like Foldable.foldr (test with [Integer])." $ do
      property ((\xs -> myFoldr (+) 0 xs == foldr (+) 0 xs) :: [Integer] -> Bool)
    it "myFoldr (implemented with foldMap) should behave like Foldable.foldr (test with Maybe Integer)." $ do
      property ((\xs -> myFoldr (+) 0 xs == foldr (+) 0 xs) :: Maybe Integer -> Bool)

-----------------------------------------------------------------
-- invoke tests in main
main :: IO ()
main = do
  test_sum
  test_product
  test_elem
  test_minimum
  test_maximum
  test_null
  test_length
  test_toList
  test_fold
  test_foldMap
  test_foldr
  