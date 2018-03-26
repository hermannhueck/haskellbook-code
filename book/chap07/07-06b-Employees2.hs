-- 07-06b-Employees2.hs
--
-- 7.6 Higher order functions, page 239
-- Employees 2, page 244
--
module Employees2 where

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank cmp e e' =
  case cmp e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'
