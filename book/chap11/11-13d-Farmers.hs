-- 11-13d-Farmers.hs
--
-- 11.13 Constructing and deconstructing values, page 421
-- Example, page 433
--
module Farmers where

newtype Name = Name String
               deriving Show
newtype Acres = Acres Int
                deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is a plain ole product of -- Name, Acres, and FarmerType
data Farmer = Farmer Name Acres FarmerType
              deriving Show

-- Now we’re going to write a very basic function that breaks down and unpacks
-- the data inside our constructors:
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- the same with record syntax
data FarmerRec =
     FarmerRec { name :: Name
               , acres :: Acres
               , farmerType :: FarmerType
               } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
        case farmerType farmer of
            DairyFarmer -> True
            _ -> False
