-- 11-18b-Ciphers.hs
--
-- 11.18 Chapter Exercises, page 452
-- Ciphers, page 453
--
module Ciphers where

-- Implement Vigenère cipher based on Ceasar cipher

import Data.Char (chr, ord, isUpper, isLower, isAlpha, toUpper, toLower)


vignere :: String -> String -> String
vignere = cipher' True

unvignere :: String -> String -> String
unvignere = cipher' False

cipher' :: Bool -> String -> String -> String
cipher' forward keyword text = map (\(ch, shift) -> encode shift ch) pairs
            where
                encode sh c
                    | not (isAlpha c) = c
                    | isLower c = chr ((ord 'a') + (sh + (ord c) - (ord 'a')) `mod` 26)
                    | otherwise = chr ((ord 'A') + (sh + (ord c) - (ord 'A')) `mod` 26)
                pairs = zip text $ map calcShift keywordSequence
                    where
                        calcShift :: Char -> Int
                        calcShift c
                                | not $ isAlpha c = 0
                                | forward = (ord $ toUpper c) - (ord 'A')
                                | otherwise = (ord 'A') - (ord $ toUpper c)
                        keywordSequence = kwSequence keyword text
                            where
                                kwSequence :: [Char] -> [Char] -> [Char]
                                kwSequence kw text = reverse $ kwSequence' (concat $ repeat kw) text []
                                        where
                                            kwSequence' :: [Char] -> [Char] -> [Char] -> [Char]
                                            kwSequence' _ [] acc = acc
                                            kwSequence' (k:ks) (t:ts) acc
                                                | isAlpha t = kwSequence' ks ts (k : acc)
                                                | otherwise = kwSequence' (k : ks) ts (t : acc)


isEqual = (unvignere "wumpel" . vignere "wumpel" $ test) == test
  where
    shift = 5
    test = "ABC , xyz"

keyword = "ALLY"
text = "MEET AT DAWN"

main :: IO ()
main = do
  print $ "Unciphering the ciphered string with the same shift value should return the original string."
  print $ show isEqual
  print $ "keyword = " ++ keyword ++ ", text = " ++ text
  print $ "ciphered text = " ++ vignere keyword text
  return ()
