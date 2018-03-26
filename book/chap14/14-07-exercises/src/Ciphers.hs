module Ciphers
  ( caesar
  , uncaesar
  , vignere
  , unvignere
  ) where

import Data.Char (chr, ord, isUpper, isLower, isAlpha, isAscii, isAsciiUpper, isAsciiLower, toUpper, toLower)

computeNewChar :: Int -> Char -> Char -> Char
computeNewChar sh c base = chr (ord base + newOrd)
      where
        newOrd = (ord c - ord base + sh) `mod` 26

encode :: Int -> Char -> Char                       -- encode a single Char
encode sh c
      | isAsciiLower c = computeNewChar sh c 'a'
      | isAsciiUpper c = computeNewChar sh c 'A'
      | otherwise = c

caesar :: Int -> String -> String
caesar _ "" = ""
caesar sh str = map (\c -> encode sh c) str
                    
uncaesar :: Int -> String -> String
uncaesar sh str = caesar (-sh) str

vignere :: String -> String -> String
vignere = cipher' True

unvignere :: String -> String -> String
unvignere = cipher' False

cipher' :: Bool -> String -> String -> String
cipher' forward keyword text = map (\(c, sh) -> encode sh c) pairs
        where
            pairs = zip text $ map (calcShift forward) $ kwSequence keyword text

calcShift :: Bool -> Char -> Int
calcShift forward c
        | (not $ isAsciiUpper c) && (not $ isAsciiLower c) = 0
        | forward = (ord $ toUpper c) - (ord 'A')
        | otherwise = (ord 'A') - (ord $ toUpper c)

kwSequence :: [Char] -> [Char] -> [Char]
kwSequence kw text = reverse $ kwSequence' (concat $ repeat kw) text []
        where
            kwSequence' :: [Char] -> [Char] -> [Char] -> [Char]
            kwSequence' _ [] acc = acc
            kwSequence' (k:ks) (t:ts) acc
                | isAlpha t = kwSequence' ks ts (k : acc)
                | otherwise = kwSequence' (k : ks) ts (t : acc)
