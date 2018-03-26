-- 03-04-TopOrLocal.hs
--
-- 3.4 Top-level versus local definitions, page 72
--
module TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x =
  x + woot + topLevelValue
  where woot :: Integer
        woot = 10

topLevelValue :: Integer
topLevelValue = 5
