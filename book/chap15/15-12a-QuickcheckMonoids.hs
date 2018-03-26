-- 15-12a-QuickcheckMonoids.hs
--
-- 15.12 Better living through QuickCheck, page 605
-- Testing associativity and identy, page 605
-- 
module QuickcheckMonoids where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

qc :: Testable prop => prop -> IO ()
qc = quickCheck

vc :: Testable prop => prop -> IO ()
vc = verboseCheck


-- property to test associativity

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssocString :: String -> String -> String -> Bool
monoidAssocString = monoidAssoc


-- properties to test left and right identity

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


main :: IO ()
main = do
    quickCheck monoidAssocString
    quickCheck (monoidLeftIdentity :: String -> Bool)
    quickCheck (monoidRightIdentity :: String -> Bool)
