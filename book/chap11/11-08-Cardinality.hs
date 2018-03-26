-- 11-08-Cardinality.hs
--
-- 11.8 What makes these datatypes algebraic?, page 403
-- Exercises: Cardinality, page 404
--
module Cardinality where  
    
-- Determine the carcinality of the following data types.

-- 1.
data PugType = PugData
-- cardinality: 1


-- 2. For this one, recall that Bool is also de ned with the |:

data Airline =
             PapuAir
           | CatapultsR'Us
           | TakeYourChancesUnited
-- cardinality: 3


-- 3. 
-- cardinality of Int8: 2 ^ 8 = 256
-- cardinality of Int16: 2 ^ 16 = 65536


-- 4.
-- cardinality of Int: 2 ^ 64 = 256
-- cardinality of Integer: infinite


-- 5. Extra credit (impress your friends!):
-- What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
-- ;-)
