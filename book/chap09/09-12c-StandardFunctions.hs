-- 09-12c-StandardFunctions.hs
--
-- 9.12 Chapter Exercises, page 339
-- Writing your own standard functions, page 341
--
module StandardFunctions where

import Test.QuickCheck
import Data.List (maximumBy, minimumBy)


----------------------------------------------------------------------------------------------
-- 0. Implementing 'and'
-- direct recursion, not using (&&)
myAnd1 :: [Bool] -> Bool
myAnd1 [] = True
myAnd1 (x:xs) = if x == False then False else myAnd1 xs

test0_1 = quickCheck ((\xs -> myAnd1 xs == and xs) :: [Bool] -> Bool)

-- direct recursion, using (&&)
myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x:xs) = x && myAnd2 xs

test0_2 = quickCheck ((\xs -> myAnd2 xs == and xs) :: [Bool] -> Bool)

-- using filter and length
myAnd3 :: [Bool] -> Bool
myAnd3 xs = length (filter (\x -> x == False) xs) == 0

test0_3 = quickCheck ((\xs -> myAnd3 xs == and xs) :: [Bool] -> Bool)


----------------------------------------------------------------------------------------------
-- 1. Implementing 'or'
-- direct recursion, not using (||)
myOr1 :: [Bool] -> Bool
myOr1 [] = False
myOr1 (x:xs) = if x == True then True else myOr1 xs

test1_1 = quickCheck ((\xs -> myOr1 xs == or xs) :: [Bool] -> Bool)

-- direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr2 xs

test1_2 = quickCheck ((\xs -> myOr2 xs == or xs) :: [Bool] -> Bool)

-- using filter and length
myOr3 :: [Bool] -> Bool
myOr3 xs = length (filter (\x -> x == True) xs) > 0

test1_3 = quickCheck ((\xs -> myOr3 xs == or xs) :: [Bool] -> Bool)


----------------------------------------------------------------------------------------------
-- 2. Implementing 'any'
-- direct recursion, not using (||)
myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 _ [] = False
myAny1 p (x:xs) = if p x == True then True else myAny1 p xs

test2_1 = quickCheck ((\xs -> myAny1 even xs == any even xs) :: [Integer] -> Bool)

-- direct recursion, using (||)
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 _ [] = False
myAny2 p (x:xs) = p x || myAny2 p xs

test2_2 = quickCheck ((\xs -> myAny2 even xs == any even xs) :: [Integer] -> Bool)

-- using filter and length
myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 p xs = length (filter (\x -> p x == True) xs) > 0

test2_3 = quickCheck ((\xs -> myAny3 even xs == any even xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 3. Implementing 'elem'
-- using 'any', works as well with any variation of myAny
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 e = any (\x -> x == e)

test3 = quickCheck ((\e xs -> myElem1 e xs == elem e xs) :: Integer -> [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 4. Implementing 'reverse'
-- using an accumulator is more efficient than appending with ++
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 xs = go xs []
  where
    go :: [a] -> [a] -> [a]
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)

test4 = quickCheck ((\xs -> myReverse1 xs == reverse xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

test5 = quickCheck ((\xs -> squish xs == concat xs) :: [[Integer]] -> Bool)


----------------------------------------------------------------------------------------------
-- 6. squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

f :: Integer -> [Integer]
f x = [x + y | y <- [10, 20, 30]]

-- same as (>>=) from type class Monad
test6_1 = quickCheck ((\xs -> squishMap f xs == (xs >>= f)) :: [Integer] -> Bool)
-- same as concatMap from type class Foldable
test6_2 = quickCheck ((\xs -> squishMap f xs == concatMap f xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 7. squishAgain  attens a list of lists into a list.
-- This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

test7 = quickCheck ((\xs -> squishAgain xs == concat xs) :: [[Integer]] -> Bool)


----------------------------------------------------------------------------------------------
-- 8. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value
-- that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy comp (x:xs) = go comp x xs
              where
                go :: (a -> a -> Ordering) -> a -> [a] -> a
                go _ m [] = m
                go comp m (x:xs) = go comp (if comp m x == GT then m else x) xs

test8 = quickCheck ((\xs -> null xs || myMaximumBy compare xs == maximumBy compare xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 9. myMinimumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value
-- that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy comp (x:xs) = go comp x xs
              where
                go :: (a -> a -> Ordering) -> a -> [a] -> a
                go _ m [] = m
                go comp m (x:xs) = go comp (if comp m x == LT then m else x) xs

test9 = quickCheck ((\xs -> null xs || myMinimumBy compare xs == minimumBy compare xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 10. myMaximum using myMaximumBy
myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

test10 = quickCheck ((\xs -> null xs || myMaximum xs == maximum xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 11. myMinimum using myMinimumBy
myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

test11 = quickCheck ((\xs -> null xs || myMinimum xs == minimum xs) :: [Integer] -> Bool)


main :: IO ()
main = do
  print $ "----- 0. myAnd ----------"
  print $ "myAnd1 [] == True  -->  " ++ show (myAnd1 [] == True)
  print $ "myAnd1 [True, True, True] == True  -->  " ++ show (myAnd1 [True, True, True] == True)
  print $ "myAnd1 [True, False, True] == False  -->  " ++ show (myAnd1 [True, False, True] == False)
  print $ "myAnd2 [] == True  -->  " ++ show (myAnd2 [] == True)
  print $ "myAnd2 [True, True, True] == True  -->  " ++ show (myAnd2 [True, True, True] == True)
  print $ "myAnd2 [True, False, True] == False  -->  " ++ show (myAnd2 [True, False, True] == False)
  print $ "myAnd3 [] == True  -->  " ++ show (myAnd3 [] == True)
  print $ "myAnd3 [True, True, True] == True  -->  " ++ show (myAnd3 [True, True, True] == True)
  print $ "myAnd3 [True, False, True] == False  -->  " ++ show (myAnd3 [True, False, True] == False)
  print $ "----- 1. myOr ----------"
  print $ "myOr1 [] == False  -->  " ++ show (myOr1 [] == False)
  print $ "myOr1 [False, False, True] == True  -->  " ++ show (myOr1 [False, False, True] == True)
  print $ "myOr1 [False, False, False] == False  -->  " ++ show (myOr1 [False, False, False] == False)
  print $ "myOr2 [] == False  -->  " ++ show (myOr2 [] == False)
  print $ "myOr2 [False, False, True] == True  -->  " ++ show (myOr2 [False, False, True] == True)
  print $ "myOr2 [False, False, False] == False  -->  " ++ show (myOr2 [False, False, False] == False)
  print $ "myOr3 [] == False  -->  " ++ show (myOr3 [] == False)
  print $ "myOr3 [False, False, True] == True  -->  " ++ show (myOr3 [False, False, True] == True)
  print $ "myOr3 [False, False, False] == False  -->  " ++ show (myOr3 [False, False, False] == False)
  print $ "----- 2. myAny ----------"
  print $ "myAny1 even [] == False  -->  " ++ show (myAny1 even [] == False)
  print $ "myAny1 even [1, 3, 5] == False  -->  " ++ show (myAny1 even [1, 3, 5] == False)
  print $ "myAny1 even [1, 3, 6] == True  -->  " ++ show (myAny1 even [1, 3, 6] == True)
  print $ "myAny2 even [] == False  -->  " ++ show (myAny2 even [] == False)
  print $ "myAny2 even [1, 3, 5] == False  -->  " ++ show (myAny2 even [1, 3, 5] == False)
  print $ "myAny2 even [1, 3, 6] == True  -->  " ++ show (myAny2 even [1, 3, 6] == True)
  print $ "myAny3 even [] == False  -->  " ++ show (myAny3 even [] == False)
  print $ "myAny3 even [1, 3, 5] == False  -->  " ++ show (myAny3 even [1, 3, 5] == False)
  print $ "myAny3 even [1, 3, 6] == True  -->  " ++ show (myAny3 even [1, 3, 6] == True)
  print $ "----- 3. myElem ----------"
  print $ "myElem1 6 [] == False  -->  " ++ show (myElem1 6 [] == False)
  print $ "myElem1 6 [1, 3, 5] == False  -->  " ++ show (myElem1 6 [1, 3, 5] == False)
  print $ "myElem1 6 [1, 3, 6] == True  -->  " ++ show (myElem1 6 [1, 3, 6] == True)
  print $ "----- 4. myReverse ----------"
  -- print $ "myReverse1 [] == []  -->  " ++ show (myReverse1 [] == []) -- does not compile ????
  print $ "reverse (myReverse1 [1, 3, 5]) == [1, 3, 5]  -->  " ++ show (reverse (myReverse1 [1, 3, 5]) == [1, 3, 5])
  print $ "reverse (myReverse1 \"blah\") == \"blah\"  -->  " ++ show (reverse (myReverse1 "blah") == "blah")
  print $ "----- 5. squish ----------"
  -- print $ "squish [] == []  -->  " ++ show (squish [] == []) -- does not compile ????
  print $ "squish [\"abc\"] == \"abc\"  -->  " ++ show (squish ["abc"] == "abc")
  print $ "squish [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squish ["abc", " mno ", "xyz"] == "abc mno xyz")
  print $ "----- 6. squishMap ----------"
  -- print $ "squishMap [] == []  -->  " ++ show (squishMap [] == []) -- does not compile ????
  print $ "squishMap (\\x -> [1, x, 3]) [2] == [1,2,3]  -->  " ++ show (squishMap (\x -> [1, x, 3]) [2] == [1,2,3])
  print $ "squishMap (\\x -> \"WO \"++[x]++\" HOO \") \"123\" [2] == \"WO 1 HOO WO 2 HOO WO 3 HOO \"  -->  " ++ show (squishMap (\x -> "WO "++[x]++" HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO ")
  print $ "----- 7. squishAgain ----------"
  -- print $ "squishAgain [] == []  -->  " ++ show (squishAgain [] == []) -- does not compile ????
  print $ "squishAgain [\"abc\"] == \"abc\"  -->  " ++ show (squishAgain ["abc"] == "abc")
  print $ "squishAgain [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squishAgain ["abc", " mno ", "xyz"] == "abc mno xyz")
  print $ "----- 8. myMaximumBy ----------"
  print $ "myMaximumBy compare [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximumBy compare [1, 53, 9001, 10] == 9001)
  print $ "----- 9. myMinimumBy ----------"
  print $ "myMinimumBy compare [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimumBy compare [1, 53, 9001, 10] == 1)
  print $ "----- 10. myMaximum ----------"
  print $ "myMaximum [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximum [1, 53, 9001, 10] == 9001)
  print $ "----- 11. myMinimum ----------"
  print $ "myMinimum [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimum [1, 53, 9001, 10] == 1)
  print $ "----- Performing tests with QuickCheck ----------"
  test0_1
  test0_2
  test0_3
  test1_1
  test1_2
  test1_3
  test2_1
  test2_2
  test2_3
  test3
  test4
  test6_1
  test6_2
  test7
  test8
  test9
  test10
  test11
  