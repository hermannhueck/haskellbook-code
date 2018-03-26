-- 11-06-Vehicles2.hs
--
-- 11.6 What’s a type and what’s data?, page 397
-- Exercises: Vehicles, page 399
--
module Vehicles2 where

data Price = Price Integer deriving (Eq, Show)

data Size = Size Double deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

-- For these exercises, we’ll use the datatypes de ned in the above section.
-- It would be good if you’d typed them all into a source file already,
-- but if you hadn’t, please do so now. You can then define
-- some sample data on your own, or use these to get you started:

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 28.45)


-- 1. What is the type of myCar?
-- Answer: myCar :: Vehicle


-- 2. Given the following, define the functions:

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- 3. Now we’re going to write a function to tell us the manufacturer of a piece of data:

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "This vehicle is not a car"


-- 4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?
-- throws an Exception


-- 5. All right. Let’s say you’ve decided to add the size of the plane
-- as an argument to the Plane constructor. Add that to your datatypes
-- in the appropriate places and change your data and functions appropriately.
-- see vehicles2.hs
