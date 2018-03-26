-- 16-06-CountingBadBreakingCompositionLaw.hs

-- 16.6 The Good, the Bad, and the Ugly, page 641
-- Composition should just work, page 643

module CountingBadBreakingCompositionLaw where

import Test.QuickCheck

data CountingBad a = Heisenberg Int a
                        deriving (Eq, Show)

-- super NOT okay
instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

main :: IO ()
main = do
    let oneWhoKnocks = Heisenberg 0 "Uncle"
        j = (++ " Jesse")
        l = (++ " lol")
    print $ "fmap (j . l) oneWhoKnocks"
    print $ fmap (j . l) oneWhoKnocks
    print $ "fmap j . fmap l $ oneWhoKnocks"
    print $ fmap j . fmap l $ oneWhoKnocks
    print $ "The results are different. The composition law does not hold."
    