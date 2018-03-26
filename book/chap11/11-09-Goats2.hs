-- 11-09-Goats2.hs
--
-- 11.9 newtype, page 406
-- Example on page 410
--
module Goats2 where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n
