-- 17-08b-ListApplicative.hs
--
-- 17.08 ZipList Monoid, page 730
-- List Applicative Exercise, page 733
--
--
module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

fromStdList :: [a] -> List a
fromStdList = foldr Cons Nil

toStdList :: List a -> [a]
toStdList = fold (:) []

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

type ListOfTriples = List (String, String, Int)

-- test with main
main :: IO ()
main = do
  hspec $ do
    describe "Simple List test with Hspec ..." $ do
      it "fromStdList [(+1), (*2)] <*> fromStdList [1, 2] `shouldBe` fromStdList [2, 3, 2, 4]" $ do
        fromStdList [(+1), (*2)] <*> fromStdList [1, 2] `shouldBe` fromStdList [2, 3, 2, 4]
  putStrLn "\nTesting List' with Checkers ..."
  putStrLn "\nquickBatch (functor (undefined :: ListOfTriples))"
  quickBatch (functor (undefined :: ListOfTriples))
  putStrLn "\nquickBatch (applicative (undefined :: ListOfTriples))"
  quickBatch (applicative (undefined :: ListOfTriples))
  putStrLn ""

-- simple console check
f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
r = f <*> v
