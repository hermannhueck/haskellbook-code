-- 23-07d-FizzBuzzWithDList2.hs
--
-- 23.7 Get a coding job with one weird trick, page 905
-- FizzBuzz (with State and DList, improved), page 908
--
module FizzBuzzWithDList2 where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist
import qualified Data.DList as DL


fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  put (DL.snoc xs (fizzBuzz n))

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList [1..100]