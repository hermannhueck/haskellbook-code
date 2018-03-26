-- 23-07f-FizzBuzzDifferently2.hs
--
-- 23.7 Get a coding job with one weird trick, page 905
-- FizzBuzz Dfferently (with State and using a reverse enum), page 908
--
module FizzBuzzDifferently2 where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList [100, 99 .. 1]