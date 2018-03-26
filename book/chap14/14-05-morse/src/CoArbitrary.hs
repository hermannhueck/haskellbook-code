-- !!! Must allow to derive type class 'Generic' !!!
{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where
    
import GHC.Generics
import Test.QuickCheck

-- !!! Must derive Generic for your own data type !!!
data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary