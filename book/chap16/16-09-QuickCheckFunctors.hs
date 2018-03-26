-- 16-09-QuickCheckFunctors.hs
--
-- 16.9 QuickChecking Functor instances, page 660
-- 
{-# LANGUAGE ViewPatterns #-}

module QuickCheckFunctors where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

main :: IO ()
main = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorIdentity :: Maybe Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: [Int] -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Maybe Int -> Bool)
  quickCheck (functorCompose' :: [Int] -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorCompose' :: Maybe Int -> IntToInt -> IntToInt -> Bool)
