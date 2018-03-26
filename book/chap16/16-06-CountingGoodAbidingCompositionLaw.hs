-- 16-06-CountingGoodAbidingCompositionLaw.hs

-- 16.6 The Good, the Bad, and the Ugly, page 641
-- Composition should just work, page 645

module CountingGoodAbidingCompositionLaw where

import Test.QuickCheck

data CountingBad a = Heisenberg Int a
                        deriving (Eq, Show)

-- Totes cool.
instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n) (f a)

main :: IO ()
main = do
    let oneWhoKnocks = Heisenberg 0 "Uncle"
        j = (++ " Jesse")
        l = (++ " lol")
    print $ "fmap (j . l) oneWhoKnocks"
    print $ fmap (j . l) oneWhoKnocks
    print $ "fmap j . fmap l $ oneWhoKnocks"
    print $ fmap j . fmap l $ oneWhoKnocks
    print $ "The results are equal. The composition law holds."
    