-- 16-10-InstancesOfFunc.hs
--
-- 16.10 Exercises: Instances of Func, page 665
-- 
-- Implement Functor instances for the following datatypes.
-- Use the QuickCheck properties we showed you to validate them.
--
{-# LANGUAGE ViewPatterns #-}

module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

-- properties for Functor laws
-- identity
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

-- composition - only values generated by QuickCheck
functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- composition - QuickCheck generates the functions as well.
functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- 1.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

quickCheck_Identity :: IO ()
quickCheck_Identity = do
  putStrLn "1. QuickChecking functor laws of 'Identity a' ..."
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Identity Int -> Bool)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)

-- 2.
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

quickCheck_Pair :: IO ()
quickCheck_Pair = do
  putStrLn "2. QuickChecking functor laws of 'Pair a a' ..."
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Pair Int -> Bool)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)

-- 3.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

quickCheck_Two :: IO ()
quickCheck_Two = do
  putStrLn "3. QuickChecking functor laws of 'Two a b' ..."
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Two Int Int -> Bool)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)

-- 4.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

quickCheck_Three :: IO ()
quickCheck_Three = do
  putStrLn "4. QuickChecking functor laws of 'Three a b c' ..."
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Three Int Int Int -> Bool)
  quickCheck
    (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)

-- 5.
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

quickCheck_Three' :: IO ()
quickCheck_Three' = do
  putStrLn "5. QuickChecking functor laws of 'Three' a b b' ..."
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Three' Int Int -> Bool)
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)

-- 6.
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z z') = Four x y z (f z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    z' <- arbitrary
    return $ Four x y z z'

quickCheck_Four :: IO ()
quickCheck_Four = do
  putStrLn "6. QuickChecking functor laws of 'Four a b c d' ..."
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Four Int Int Int Int -> Bool)
  quickCheck
    (functorCompose' :: Four Int Int Int Int -> IntToInt -> IntToInt -> Bool)

-- 7.
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z z') = Four' x y z (f z')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    z' <- arbitrary
    return $ Four' x y z z'

quickCheck_Four' :: IO ()
quickCheck_Four' = do
  putStrLn "7. QuickChecking functor laws of 'Four' a a a b' ..."
  quickCheck (functorIdentity :: Four' Int Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Four' Int Int -> Bool)
  quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)

-- 8. Can you implement one for this type? Why? Why not?
data Trivial =
  Trivial
  deriving (Eq, Show)

quickCheck_Trivial :: IO ()
quickCheck_Trivial = do
  putStrLn "8. QuickChecking functor laws of 'Trivial' ..."
  putStrLn "data Trivial = Trivial deriving (Eq, Show)"
  putStrLn "'Trivial' cannot have a Functor instance."
  putStrLn "'It's kind is *, but should be * -> *"

main :: IO ()
main = do
  quickCheck_Identity
  quickCheck_Pair
  quickCheck_Two
  quickCheck_Three
  quickCheck_Three'
  quickCheck_Four
  quickCheck_Four'
  quickCheck_Trivial