-- 11-18c-AsPatterns.hs
--
-- 11.18 Chapter Exercises, page 452
-- As-Patterns, page 454
--
module AsPatterns where

import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t

    
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs


-- Remember that the sub-sequence has to be in the original order!

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf all@(x : xs) (y : ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf all ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(c : cs) -> (w, toUpper c : cs)) . words


main :: IO ()
main = do
    print "----- main ----------"
    return ()
