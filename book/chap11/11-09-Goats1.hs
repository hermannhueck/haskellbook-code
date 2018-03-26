-- 11-09-Goats1.hs
--
-- 11.9 newtype, page 406
-- Example on page 408
--
module Goats1 where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 43
