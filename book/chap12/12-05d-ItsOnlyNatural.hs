-- 12-05d-ItsOnlyNatural.hs
--
-- 12.5 Chapter Exercises, page 480
-- It’s only Natural, page 482
--
module ItsOnlyNatural where
    
import Data.Char
import Data.Maybe
    
-- You’ll be presented with a datatype to represent the natural numbers.
-- The only values representable with the naturals are whole numbers
-- from zero to infinity. Your task will be to implement functions
-- to convert Naturals to Integers and Integers to Naturals.
-- The conversion from Naturals to Integers won’t return Maybe
-- because Integer is a strict superset of Natural. Any Natural
-- can be represented by an Integer, but the same is not true
-- of any Integer. Negative numbers are not valid natural numbers.


-- As natural as any competitive bodybuilder

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing

integerToNat :: Integer -> Maybe Nat
integerToNat i
        | i < 0         = Nothing
        | i == 0        = Just Zero
        | otherwise     = Just $ Succ $ fromJust $ integerToNat $ i - 1
