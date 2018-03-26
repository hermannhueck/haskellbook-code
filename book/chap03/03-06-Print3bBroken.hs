-- 03-06-Print3bBroken.hs
--
-- 3.6 Concatenation and scoping, page 75
-- Print3bBroken, page 77
--
module Print3bBroken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Yarrrrr"
