-- 15-12c-MaybeAnotherMonoid.hs
--
-- 15.12 Better living through QuickCheck, page 605
-- Exercise: Maybe Another Monoid, page 611
--
module MaybeAnotherMonoid where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

qc :: Testable prop => prop -> IO ()
qc = quickCheck

vc :: Testable prop => prop -> IO ()
vc = verboseCheck


-- Optional from prev exercise needs an additional Arbitrary for QuickCheck

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)


instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = arbitrary >>=
        \a -> frequency [(1, return Nada),
                         (1, return (Only a))]

instance Monoid a => Monoid (Optional a) where
    mempty                      = Nada
    mappend (Only x) (Only y)   = Only $ x <> y
    mappend opt Nada            = opt
    mappend Nada opt            = opt


-- First'

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        return (First' a)
    
instance Monoid (First' a) where
    mempty                          = First' (Nada)
    mappend (First' (Only x)) _     = First' (Only x)
    mappend (First' Nada) y         = y
    
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend


-- monoid properties

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- Test with QuickCheck

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
