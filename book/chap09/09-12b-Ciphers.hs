-- 09-12b-Ciphers.hs
--
-- 9.12 Chapter Exercises, page 339
-- Ciphers, page 340
--
module Ciphers where

import Data.Char (chr, isAlpha, isLower, isUpper, ord)

caesar :: Int -> String -> String
caesar _ "" = ""
caesar shift str = map (\char -> encode shift char) str
  where
    encode :: Int -> Char -> Char
    encode shift c
      | not (isAlpha c) = c
      | isLower c = chr $ newOrd shift (ord c) (ord 'a')
      | otherwise = chr $ newOrd shift (ord c) (ord 'A')
      where
        newOrd :: Int -> Int -> Int -> Int
        newOrd shift ordChar ordBase =
          ((ordChar - ordBase + shift) `mod` 26) + ordBase

uncaesar :: Int -> String -> String
uncaesar shift str = caesar (-shift) str

isEqual = (uncaesar shift . caesar shift $ test) == test
  where
    shift = 5
    test = "ABC , xyz"

main :: IO ()
main = do
  print $ "Uncaesaring the caesared string with the same shift value should return the original string."
  print $ show isEqual
