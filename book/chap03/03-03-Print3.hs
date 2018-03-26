-- 03-03-Print3.hs
--
-- 3.3 Printing simple strings, page 67
-- Print3, page 70
--
module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting =
          concat [hello, " ", world]
