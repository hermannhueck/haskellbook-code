-- 17-09b-Ex2-ApplicativeInstances.hs
--
-- 17.09 Chapter Exercises, page 741
-- Applicative Instances, page 741
--
-- Write instance for the following datatypes. Confused? Write out
-- what the type should be. Use the checkers library to validate the instances.
--
module ApplicativeInstances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---------------------------------------------------------------------------------------------
-- 1. Pair
data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y)         = Pair (f x) (f y)

instance Applicative Pair where
  pure x                    = Pair x x
  Pair f g <*> Pair x y     = Pair (f x) (g y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

type TypeApplicativePair = Pair (String, String, Int)

quickBatch_Pair :: IO ()
quickBatch_Pair = do
  putStrLn "\n----- Pair a --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativePair))"
  quickBatch (functor (undefined :: TypeApplicativePair))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativePair))"
  quickBatch (applicative (undefined :: TypeApplicativePair))

---------------------------------------------------------------------------------------------
-- 2. Two
data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y)          = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure y                    = Two mempty y
  Two x f <*> Two x' y      = Two (x <> x') (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

type TypeApplicativeTwo = Two [Int] (String, String, Int)

quickBatch_Two :: IO ()
quickBatch_Two = do
  putStrLn "\n----- Two a b --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativeTwo))"
  quickBatch (functor (undefined :: TypeApplicativeTwo))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativeTwo))"
  quickBatch (applicative (undefined :: TypeApplicativeTwo))

---------------------------------------------------------------------------------------------
-- 3. Three
data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z)            = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure z                          = Three mempty mempty z
  Three x y f <*> Three x' y' z   = Three (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type TypeApplicativeThree = Three [Int] (Sum Int) (String, String, Int)

quickBatch_Three :: IO ()
quickBatch_Three = do
  putStrLn "\n----- Three a b c --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativeThree))"
  quickBatch (functor (undefined :: TypeApplicativeThree))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativeThree))"
  quickBatch (applicative (undefined :: TypeApplicativeThree))

---------------------------------------------------------------------------------------------
-- 4. Three'
data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z)             = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure z                            = Three' mempty z z
  Three' x f g <*> Three' x' y z    = Three' (x <> x') (f y) (g z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

type TypeApplicativeThree' = Three' [Int] (String, String, Int)

quickBatch_Three' :: IO ()
quickBatch_Three' = do
  putStrLn "\n----- Three' a b c --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativeThree'))"
  quickBatch (functor (undefined :: TypeApplicativeThree'))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativeThree'))"
  quickBatch (applicative (undefined :: TypeApplicativeThree'))

---------------------------------------------------------------------------------------------
-- 5. Four
data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z z')                = Four x y z (f z')

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure z'                               = Four mempty mempty mempty z'
  Four x y z f <*> Four x' y' z' z''    = Four (x <> x') (y <> y') (z <> z') (f z'')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

type TypeApplicativeFour = Four [String] (Sum Int) (Product Int) (String, String, Int)

quickBatch_Four :: IO ()
quickBatch_Four = do
  putStrLn "\n----- Four a b c d --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativeFour))"
  quickBatch (functor (undefined :: TypeApplicativeFour))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativeFour))"
  quickBatch (applicative (undefined :: TypeApplicativeFour))

---------------------------------------------------------------------------------------------
-- 6. Four'
data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z z')                 = Four' x y z (f z')

instance (Monoid a) => Applicative (Four' a) where
  pure z'                                 = Four' mempty mempty mempty z'
  Four' x y z f <*> Four' x' y' z' z''    = Four' (x <> x') (y <> y') (z <> z') (f z'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

type TypeApplicativeFour' = Four' [Int] (String, String, Int)

quickBatch_Four' :: IO ()
quickBatch_Four' = do
  putStrLn "\n----- Four' a b --------------"
  putStrLn "quickBatch (functor (undefined :: TypeApplicativeFour'))"
  quickBatch (functor (undefined :: TypeApplicativeFour'))
  putStrLn "\nquickBatch (applicative (undefined :: TypeApplicativeFour'))"
  quickBatch (applicative (undefined :: TypeApplicativeFour'))

---------------------------------------------------------------------------------------------
-- test with main
main :: IO ()
main = do
  putStrLn "\n----- Testing Applicative instances with Checkers --------------"
  quickBatch_Pair
  quickBatch_Two
  quickBatch_Three
  quickBatch_Three'
  quickBatch_Four
  quickBatch_Four'
  putStrLn ""
