module WordNumber
    ( digits
    , digitToWord
    , wordNumber
    ) where

import Data.List(intersperse)

digits :: Int -> [Int]
digits n = reverse (reversedDigits n)
  where reversedDigits x
          | x < 10 = [x]
          | otherwise = (x `mod` 10) : reversedDigits (x `div` 10)

digitToWord :: Int -> String
digitToWord d = case d of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error ("illegal digit: " ++ show d)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
