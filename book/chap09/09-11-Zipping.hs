-- 09-11-Zipping.hs
--
-- 9.11 Zipping lists, page 336
-- Zipping Exercises, page 338
--
module Zipping where

import Test.QuickCheck

-- 1. Write your own version of zip and ensure it behaves the same as the original.
myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip _ _ = []

-- 2. Do what you did for zip, but now for zipWith:
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

-- 3. Rewrite your zip in terms of the zipWith you wrote.
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (\x -> \y -> (x,y))

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Testing myZip ..."
    quickCheck ((\xs ys -> myZip xs ys == zip xs ys) :: [Integer] -> [Integer] -> Bool)
    quickCheck ((\xs ys -> myZip xs ys == zip xs ys) :: [String] -> [String] -> Bool)
    putStrLn ""
    putStrLn "Testing myZipwith ..."
    quickCheck ((\xs ys -> myZipWith (+) xs ys == zipWith (+) xs ys) :: [Integer] -> [Integer] -> Bool)
    quickCheck ((\xs ys -> myZipWith (++) xs ys == zipWith (++) xs ys) :: [String] -> [String] -> Bool)
    putStrLn ""
    putStrLn "Testing myZip' ..."
    quickCheck ((\xs ys -> myZip' xs ys == zip xs ys) :: [Integer] -> [Integer] -> Bool)
    quickCheck ((\xs ys -> myZip' xs ys == zip xs ys) :: [String] -> [String] -> Bool)
    