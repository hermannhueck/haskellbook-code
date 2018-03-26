module FoolGen where
    
import Test.QuickCheck
import Test.QuickCheck.Gen


data Fool = Fulse | Frue deriving (Eq, Show)

-- 1. Equal probabilities for each.

foolGenWithDistributionRatio1To1 :: Gen Fool
foolGenWithDistributionRatio1To1 = do
    oneof [return $ Fulse,
            return $ Frue]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.

foolGenWithDistributionRatio2To1 :: Gen Fool
foolGenWithDistributionRatio2To1 = do
    frequency [(2, return $ Fulse),
               (1, return $ Frue)]

-- 3. 9/10s chance of Fulse, 1/10 chance of Frue.

foolGenWithDistributionRatio9To1 :: Gen Fool
foolGenWithDistributionRatio9To1 = do
    frequency $ zip [9, 1] $ map return [Fulse, Frue]
