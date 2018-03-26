-- 09-07a-ComprehendThyLists.hs
--
-- 9.7 List comprehensions, page 312
-- Exercises: Comprehend Thy Lists, page 315
--
module ComprehendThyLists where

mySqr = [x^2 | x <- [1..10]]
myEven = [x | x <- mySqr, rem x 2 == 0]
tuples = [(x, y) |
        x <- mySqr,
        y <- mySqr,
        x < 50, y > 50]
take5 = take 5 tuples

main :: IO ()
main = do
  print $ "mySqr  = " ++ show mySqr
  print $ "myEven = " ++ show myEven
  print $ "tuples = " ++ show tuples
  print $ "take5  = " ++ show take5
