-- 23-07e-FizzBuzzDifferently.hs
--
-- 23.7 Get a coding job with one weird trick, page 905
-- FizzBuzz Dfferently (without State), page 908
--
module FizzBuzzDifferently where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = go from to []
  where
    go :: Integer -> Integer -> [String] -> [String]
    go from' to' acc
      | to' < from' = acc
      | otherwise = go from' (to'-1) (fizzBuzz to' : acc)

-- a better solution
fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' from to
  | from > to = []
  | from == to = [fizzBuzz from]
  | otherwise = fizzBuzz from : fizzbuzzFromTo' (from+1) to

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo' 1 100