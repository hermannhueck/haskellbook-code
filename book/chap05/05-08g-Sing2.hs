-- 05-08g-Sing2.hs
--
-- 5.8 Chapter Exercises, page 147
-- Fix it - Sing 2, page 154
--
module Sing2 where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing =
  if (x < y)
    then fstString x
    else sndString y
  where
    x = "Singin"
    y = "Somewhere"
