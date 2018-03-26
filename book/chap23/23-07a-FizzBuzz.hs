-- 23-07a-FizzBuzz.hs
--
-- 23.7 Get a coding job with one weird trick, page 905
-- FizzBuzz (without State), page 905
--
module FizzBuzz where

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]