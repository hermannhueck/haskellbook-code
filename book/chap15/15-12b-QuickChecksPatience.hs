-- 15-12b-QuickChecksPatience.hs
--
-- 15.12 Better living through QuickCheck, page 605
-- Testing QuickChecks patience, page 608
-- 
module QuickChecksPatience where

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


-- properties to test left and right identity

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Bull = Fools | Twoo
            deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [
                      (1, return Fools)
                    , (1, return Twoo)
                    ]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mlr = monoidRightIdentity
    quickCheck (ma :: BullMappend)
    quickCheck $ expectFailure (mli :: Bull -> Bool)
    quickCheck $ expectFailure (mlr :: Bull -> Bool)
