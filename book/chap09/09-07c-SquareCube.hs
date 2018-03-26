-- 09-07b-SquareCube.hs
--
-- 9.7 List comprehensions, page 312
-- Exercises: Square Cube, page 317
--
module SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
zipped = [(x,y) | x <- mySqr, y <- myCube]
zippedLt50 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
tuples = [(x, y) |
        x <- mySqr,
        y <- mySqr,
        x < 50, y > 50]
take5 = take 5 tuples

main :: IO ()
main = do
  print $ "mySqr      = " ++ show mySqr
  print $ "myCube     = " ++ show myCube
  print $ "zipped     = " ++ show zipped
  print $ "length zipped = " ++ show (length zipped)
  print $ "zippedLt50 = " ++ show zippedLt50
  print $ "length zippedLt50 = " ++ show (length zippedLt50)
