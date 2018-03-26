-- 09-05-EnumFromTo.hs
--
-- 9.5 Using ranges to construct lists, page 305
-- Exercise: EnumFromTo, page 307
--
module EnumFromTo where


eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftBool (succ x) y


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftOrd (succ x) y


eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftInt (succ x) y


eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftChar (succ x) y


-- generic solution for all types
eftEnum :: (Eq a, Ord a, Enum a) => a -> a -> [a]
eftEnum x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eftEnum (succ x) y
