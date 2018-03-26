-- 03-08c-Reverse.hs
--
-- 3.8 Chapter Exercises, page 81
-- Reverse, page 84
--
module Reverse where

import Data.List

-- 6. rvrs in it's own module
rvrs :: String -> String
rvrs str = concat [s3, " ", s2, " ", s1]
  where
    s1 = take 5 str
    s2 = take 2 $ drop 6 str
    s3 = drop 9 str

rvrs' :: String -> String
rvrs' str = concat $ intersperse " " $ reverse $ words str

main :: IO ()
main = do
  let s = "Curry is awesome"
      s' = "This is another sentence"
  print (rvrs s)
  print $ rvrs s
  print (rvrs' s)
  print $ rvrs' s
  print (rvrs' s')
  print $ rvrs' s'
