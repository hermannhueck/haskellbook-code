-- 11-05-DogTypes.hs
--
-- 11.5 Data constructors and values, page 393
-- Exercises: Dog Types, page 396
--
module DogTypes where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData
-- no witness to the contrary ^

-- This will work because the value 10 agrees with the type variable being bound to Int:
myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- This will not work because 10 cannot be reconciled with the type variable being bound to String:
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a =
  Husky a
  | Mastiff a deriving (Eq, Show)


-- Given the datatypes de ned in the above sections,

-- 1. Is Doggies a type constructor or a data constructor?
-- Answer: type constructor


-- 2. What is the kind of Doggies?
-- Answer: Doggies :: * -> *


-- 3. What is the kind of Doggies String?
-- Answer: Doggies String :: *


-- 4. What is the type of Husky 10?
-- Answer: Husky 10 :: Num a => Doggies a


-- 5. What is the type of Husky (10 :: Integer)?
-- Answer: Husky (10 :: Integer) :: Doggies Integer


-- 6. What is the type of Mastiff "Scooby Doo"?
-- Answer: Mastiff "Scooby Doo" :: Doggies [Char]


-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- Answer: type constructor AND data constructor


-- 8. What is the type of DogueDeBordeaux?
-- Answer: DogueDeBordeaux :: doge -> DogueDeBordeaux doge


-- 9. What is the type of DogueDeBordeaux "doggie!"?
-- Answer: DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
