-- 16-06-WhoCaresAbidingIdentityLaw.hs

-- 16.6 The Good, the Bad, and the Ugly, page 641
-- WhoCares, page 642

module WhoCaresAbidingIdentityLaw where

import Test.QuickCheck

data WhoCares a =
    ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)
 
instance Functor WhoCares where
    fmap _ ItDoesnt = WhatThisIsCalled
    fmap _ WhatThisIsCalled = ItDoesnt
    fmap f (Matter a) = Matter (f a)

instance Arbitrary a => Arbitrary (WhoCares a) where
    arbitrary = do
        x <- arbitrary
        frequency
            $ zip [1,3,1]
            $ map return [ItDoesnt, Matter x, WhatThisIsCalled]

functorIdentityLaw :: Eq a => WhoCares a -> Bool
functorIdentityLaw f = fmap id f == id f

main :: IO ()
main = do
    quickCheck $ expectFailure (functorIdentityLaw :: WhoCares Int -> Bool)