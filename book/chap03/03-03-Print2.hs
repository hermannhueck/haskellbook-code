-- 03-03-Print2.hs
--
-- 3.3 Printing simple strings, page 67
-- Print2, page 69
--
module Print2 where
  
main :: IO ()
main = do
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
