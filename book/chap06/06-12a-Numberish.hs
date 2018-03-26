-- 06-12-Numberish.hs
--
-- 6.12 Instances are dispatched by type, page 199
-- Numberish, page 200
--
module Numberish where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

-- pretend newtype is data for now
newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime
