-- 18-04a-TwiceWhenEven.hs
--
-- 18.04 Examples of Monad use, page 759
-- Example of List Monad in use, page 760
--
--
module TwiceWhenEven where

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

result = twiceWhenEven [1..3]
result' = twiceWhenEven' [1..3]

test = result == [1, 4, 4, 9] && result' == [4, 4]
