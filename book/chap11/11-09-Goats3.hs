-- 11-09-Goats3.hs

-- 11.9 newtype, page 403
-- 2nd Example on page 410
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Goats3 where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
