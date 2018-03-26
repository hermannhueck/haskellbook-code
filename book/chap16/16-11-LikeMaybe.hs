-- 16-11-LikeMaybe.hs
--
-- 16.11 Ignoring possibilities, page 663
-- Exercise: Possibly, page 666
--
-- Write a Functor instance for a datatype identical to Maybe. We’ll use our own datatype
-- because Maybe already has a Functor instance and we cannot make a duplicate one.
module LikeMaybe where

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

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    oneof $ map return [LolNope, Yeppers x]

main :: IO ()
main = do
  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck ((functorCompose (+ 1) (* 2)) :: Possibly Int -> Bool)
  quickCheck (functorCompose' :: Possibly Int -> IntToInt -> IntToInt -> Bool)
