-- 09-07b-Acronyms.hs
--
-- 9.7 List comprehensions, page 312
-- Acronyms, page 316
--
module Acronyms where

import Data.Char (isUpper)

scuba = "Self Contained Underwater Breathing Apparatus"
nasa = "National Aeronautics and Space Administration"

acro xs = [x | x <- xs, elem x ['A'..'Z']]

acro' xs = [x | x <- xs, isUpper x]

filterList predicate xs = [x | x <- xs, predicate x]

acro'' = filterList isUpper

main :: IO ()
main = do
  putStrLn ""
  print $ "acro " ++ scuba ++ " = " ++ (acro scuba)
  print $ "acro " ++ nasa ++ " = " ++ (acro nasa)
  putStrLn ""
  print $ "acro' " ++ scuba ++ " = " ++ (acro' scuba)
  print $ "acro' " ++ nasa ++ " = " ++ (acro' nasa)
  putStrLn ""
  print $ "acro'' " ++ scuba ++ " = " ++ (acro'' scuba)
  print $ "acro'' " ++ nasa ++ " = " ++ (acro'' nasa)
