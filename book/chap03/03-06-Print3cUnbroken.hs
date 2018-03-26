-- 03-06-Print3cUnbroken.hs
--
-- 3.6 Concatenation and scoping, page 75
-- Print3cUnbroken, page 77
--
module Print3cUnbroken where

greeting :: String
greeting = "Yarrrrr"
-- can also be defined locally in printSecond

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where greeting = "Yarrrrr"
