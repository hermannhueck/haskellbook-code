-- 13-14a-Ciphers.hs
--
-- 13.14 Chapter Exercises, page 531
-- Modifying code, Ciphers, page 332
--
module Ciphers where

import Data.Char (chr, ord, isUpper, isLower, isAlpha)

-- 1. Ciphers: Open your Ciphers module and modify it so that the Caesar and Vigenère ciphers work with user input.

caesar :: Int -> String -> String
caesar _ "" = ""
caesar shift str = map (\ch -> encode shift ch) str
            where encode sh c
                    | not (isAlpha c) = c
                    | isLower c = chr ((ord 'a') + (sh + (ord c) - (ord 'a')) `mod` 26)
                    | otherwise = chr ((ord 'A') + (sh + (ord c) - (ord 'A')) `mod` 26)

uncaesar :: Int -> String -> String
uncaesar shift str = caesar (-shift) str


isEqual = (uncaesar shift . caesar shift $ test) == test
  where
    shift = 5
    test = "ABC , xyz"


main :: IO ()
main = do
  print $ "--- Enter the shift number:"
  shiftStr <- getLine
  let shift = read shiftStr :: Int
  print $ "--- Enter the text to be ceasar ciphered:"
  text <- getLine
  print $ "--- The ciphered text is:"
  print $ caesar shift text
  return ()
