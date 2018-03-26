-- 06-05b-Date.hs
--
-- 6.5 Writing typeclass instances, page 169
-- Date, page 172
--
module Date where

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek Int
  deriving (Show)

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'
