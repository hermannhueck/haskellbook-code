-- 07-09-Arith3.hs
--
-- 7.9 Pointfree style, page 257
-- Arith3, page 258
-- (same with $ syntax)
--
module Arith3 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

-- using $ syntax instead of parenthesis
main :: IO ()
main = do
  print (0 :: Int)
  print $ add 1 0
  print $ addOne 0
  print $ addOnePF 0
  print $ addOne . addOne $ 0
  print $ addOnePF . addOne $ 0
  print $ addOne . addOnePF $ 0
  print $ addOnePF . addOnePF $ 0
  print $ negate $ addOne 0
  print $ negate . addOne $ 0
  print $ addOne . addOne . addOne . negate . addOne $ 0
