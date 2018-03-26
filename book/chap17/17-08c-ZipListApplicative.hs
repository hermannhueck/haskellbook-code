-- 17-08c-ZipListApplicative.hs
--
-- 17.08 ZipList Monoid, page 730
-- ZipList Applicative Exercise, page 736
--
--
module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List
--
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

fromStdList :: [a] -> List a
fromStdList = foldr Cons Nil

toStdList :: List a -> [a]
toStdList = fold (:) []
  
take' :: Int -> List a -> List a
take' 0 xs            = Nil
take' n Nil           = Nil
take' n (Cons x xs)   = Cons x (take' (n-1) xs)

zip' :: List (a -> b) -> List a -> List b
zip' Nil _                   = Nil
zip' _ Nil                   = Nil
zip' (Cons f fs) (Cons x xs) = Cons (f x) $ zip' fs xs

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

instance Monoid (List a) where
  mempty                      = Nil
  Nil `mappend` ys            = ys
  (Cons x xs) `mappend` ys    = Cons x (xs `mappend` ys)

instance Functor List where
  fmap _ Nil                  = Nil
  fmap f (Cons x xs)          = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x                      = Cons x Nil
  Nil <*> _                   = Nil
  _ <*> Nil                   = Nil
  (Cons f fs) <*> xs          = (fmap f xs) `mappend` (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    frequency $
        zip [1, 9] $
        return <$> [Nil, Cons x Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- ZipList'
--
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  ZipList' fs <*> ZipList' xs = ZipList' $ fs `zip'` xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

type ListOfTriples = List (String, String, Int)
type ZipListOfTriples = ZipList' (String, String, Int)

-- test with main
main :: IO ()
main = do
  hspec $ do
    describe "Simple ZipList' test with Hspec ..." $ do
      it "ZipList' (fromStdList [(+9), (*2), (+8)]) <*> ZipList' (fromStdList [1..3]) `shouldBe` ZipList' (fromStdList [10, 4, 11])" $ do
        ZipList' (fromStdList [(+9), (*2), (+8)]) <*> ZipList' (fromStdList [1..3]) `shouldBe` ZipList' (fromStdList [10, 4, 11])
      it "ZipList' (fromStdList [(+9), (*2), (+8)]) <*> ZipList' (repeat' 1) `shouldBe` ZipList' (fromStdList [10, 2, 9])" $ do
        ZipList' (fromStdList [(+9), (*2), (+8)]) <*> ZipList' (repeat' 1) `shouldBe` ZipList' (fromStdList [10, 2, 9])
  putStrLn "\nTesting List with Checkers ..."
  putStrLn "\nquickBatch (functor (undefined :: ListOfTriples))"
  quickBatch (functor (undefined :: ListOfTriples))
  putStrLn "\nquickBatch (applicative (undefined :: ListOfTriples))"
  quickBatch (applicative (undefined :: ListOfTriples))
  putStrLn "\nTesting ZipList' with Checkers ..."
  putStrLn "\nquickBatch (functor (undefined :: ZipListOfTriples))"
  quickBatch (functor (undefined :: ZipListOfTriples))
  putStrLn "\nquickBatch (applicative (undefined :: ZipListOfTriples))"
  quickBatch (applicative (undefined :: ZipListOfTriples))
  putStrLn ""

-- simple console check
z1 = ZipList' (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
z2 = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
z3 = ZipList' $ repeat' 1

z12 = z1 <*> z2
z13 = z1 <*> z3
