-- 10-10b-StandardFunctionsWithFold.hs
--
-- 10.10 Chapter Exercises, page 379
-- Rewriting functions using folds, page 380
--
module StandardFunctionsWithFold where


import Test.QuickCheck
import Data.List (maximumBy, minimumBy)
import Data.Char
import Data.Bool


-- In the previous chapter, you wrote these functions using direct recursion over lists.
-- The goal now is to rewrite them using folds. Where possible, to gain a deeper
-- understanding of folding, try rewriting the fold version so that it is point-free.
-- Point-free versions of these functions written with a fold should look like:

-- myFunc = foldr f z


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

-- fold, not point-free
-- in the folding function
myAnd4 :: [Bool] -> Bool
myAnd4 = foldr (\a b -> if a == False then False else b) True

test0_4 = quickCheck ((\xs -> myAnd4 xs == and xs) :: [Bool] -> Bool)

-- fold, both myAnd and the folding function are point-free now
myAnd5 :: [Bool] -> Bool
myAnd5 = foldr (&&) True

test0_5 = quickCheck ((\xs -> myAnd5 xs == and xs) :: [Bool] -> Bool)


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

-- with fold in point-free style
myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) False

test1_4 = quickCheck ((\xs -> myOr4 xs == or xs) :: [Bool] -> Bool)


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

-- with fold in point-free style
myAny4 :: (a -> Bool) -> [a] -> Bool
myAny4 p = foldr (\x y -> p x || y) False

test2_4 = quickCheck ((\xs -> myAny4 even xs == any even xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 3. Implementing 'elem'
-- using 'any', works as well with any variation of myAny
myElem1 :: Eq a => a -> [a] -> Bool
myElem1 e = any (\x -> x == e)

test3_1 = quickCheck ((\e xs -> myElem1 e xs == elem e xs) :: Integer -> [Integer] -> Bool)

-- shrt versionmof myElem1
myElem2 :: Eq a => a -> [a] -> Bool
myElem2 e = any (== e)

test3_2 = quickCheck ((\e xs -> myElem2 e xs == elem e xs) :: Integer -> [Integer] -> Bool)

-- with fold in point-free style
myElem3 :: Eq a => a -> [a] -> Bool
myElem3 e = foldr (\x y -> (e == x) || y) False

test3_3 = quickCheck ((\e xs -> myElem3 e xs == elem e xs) :: Integer -> [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 4. Implementing 'reverse'
-- using accumulator is more efficient than appending with ++
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 xs = go xs []
  where
    go :: [a] -> [a] -> [a]
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)

test4_1 = quickCheck ((\xs -> myReverse1 xs == reverse xs) :: [Integer] -> Bool)

-- with fold in point-free style
myReverse2 :: [a] -> [a]
myReverse2 = foldr (\x y -> y ++ [x]) []

test4_2 = quickCheck ((\xs -> myReverse2 xs == reverse xs) :: [Integer] -> Bool)

myReverse3 :: [a] -> [a]
myReverse3 = foldl (flip (:)) []

test4_3 = quickCheck ((\xs -> myReverse3 xs == reverse xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f = foldr (\x y -> f x : y) []

test5_1 = quickCheck ((\xs -> myMap1 (^2) xs == map (^2) xs) :: [Integer] -> Bool)

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr ((:) . f) []

test5_2 = quickCheck ((\xs -> myMap2 (^2) xs == map (^2) xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter.
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 p = foldr (\x y -> if p x then x : y else y) []

test6_1 = quickCheck ((\xs -> myFilter1 even xs == filter even xs) :: [Integer] -> Bool)

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p = foldr (\x y -> bool y (x:y) (p x)) []

test6_2 = quickCheck ((\xs -> myFilter2 even xs == filter even xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 7. squish flattens a list of lists into a list
squish1 :: [[a]] -> [a]
squish1 [] = []
squish1 (l:ls) = l ++ squish1 ls

test7_1 = quickCheck ((\xs -> squish1 xs == concat xs) :: [[Integer]] -> Bool)

-- with fold in point-free style
squish2 :: [[a]] -> [a]
squish2 = foldr (++) []

test7_2 = quickCheck ((\xs -> squish2 xs == concat xs) :: [[Integer]] -> Bool)


----------------------------------------------------------------------------------------------
-- 8. squishMap maps a function over a list and concatenates the results
squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 _ [] = []
squishMap1 f (x:xs) = f x ++ squishMap1 f xs

f :: Integer -> [Integer]
f x = [x + y | y <- [10, 20, 30]]

-- same as (>>=) from type class Monad
test8_1a = quickCheck ((\xs -> squishMap1 f xs == (xs >>= f)) :: [Integer] -> Bool)
-- same as concatMap from type class Foldable
test8_1b = quickCheck ((\xs -> squishMap1 f xs == concatMap f xs) :: [Integer] -> Bool)

-- with fold in point-free style
squishMap2 :: (a -> [b]) -> [a] -> [b]
squishMap2 f = foldr (\x y -> f x ++ y) []

test8_2a = quickCheck ((\xs -> squishMap2 f xs == (xs >>= f)) :: [Integer] -> Bool)
test8_2b = quickCheck ((\xs -> squishMap2 f xs == concatMap f xs) :: [Integer] -> Bool)

squishMap3 :: (a -> [b]) -> [a] -> [b]
squishMap3 f = foldr ((++) . f) []

test8_3a = quickCheck ((\xs -> squishMap3 f xs == (xs >>= f)) :: [Integer] -> Bool)
test8_3b = quickCheck ((\xs -> squishMap3 f xs == concatMap f xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 9. squishAgain  attens a list of lists into a list.
-- This time re-use the squishMap1 function.
squishAgain1 :: [[a]] -> [a]
squishAgain1 = squishMap1 id

test9_1 = quickCheck ((\xs -> squishAgain1 xs == concat xs) :: [[Integer]] -> Bool)

-- same as above using the squishMap2 function
squishAgain2 :: [[a]] -> [a]
squishAgain2 = squishMap2 id

test9_2 = quickCheck ((\xs -> squishAgain2 xs == concat xs) :: [[Integer]] -> Bool)

-- same as above using the squishMap3 function
squishAgain3 :: [[a]] -> [a]
squishAgain3 = squishMap3 id

test9_3 = quickCheck ((\xs -> squishAgain3 xs == concat xs) :: [[Integer]] -> Bool)


----------------------------------------------------------------------------------------------
-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value
-- that the comparison returned GT for.
myMaximumBy1 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy1 comp (x:xs) = go comp x xs
              where
                go :: (a -> a -> Ordering) -> a -> [a] -> a
                go _ m [] = m
                go comp m (x:xs) = go comp (if comp m x == GT then m else x) xs

test10_1 = quickCheck ((\xs -> null xs || myMaximumBy1 compare xs == maximumBy compare xs) :: [Integer] -> Bool)

-- with fold
myMaximumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy2 comp (x:xs) = foldr (\m x -> if comp m x == GT then m else x) x xs

test10_2 = quickCheck ((\xs -> null xs || myMaximumBy2 compare xs == maximumBy compare xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 11. myMinimumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value
-- that the comparison returned LT for.
myMinimumBy1 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy1 comp (x:xs) = go comp x xs
              where
                go :: (a -> a -> Ordering) -> a -> [a] -> a
                go _ m [] = m
                go comp m (x:xs) = go comp (if comp m x == LT then m else x) xs

test11_1 = quickCheck ((\xs -> null xs || myMinimumBy1 compare xs == minimumBy compare xs) :: [Integer] -> Bool)

-- with fold
myMinimumBy2 :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy2 comp (x:xs) = foldr (\m x -> if comp m x == LT then m else x) x xs

test11_2 = quickCheck ((\xs -> null xs || myMinimumBy2 compare xs == minimumBy compare xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 12. myMaximum using myMaximumBy
myMaximum1 :: Ord a => [a] -> a
myMaximum1 = myMaximumBy1 compare

test12_1 = quickCheck ((\xs -> null xs || myMaximum1 xs == maximum xs) :: [Integer] -> Bool)

-- same as above using the myMaximumBy2 function
myMaximum2 :: Ord a => [a] -> a
myMaximum2 = myMaximumBy2 compare

test12_2 = quickCheck ((\xs -> null xs || myMaximum2 xs == maximum xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- 13. myMinimum using myMinimumBy
myMinimum1 :: Ord a => [a] -> a
myMinimum1 = myMinimumBy1 compare

test13_1 = quickCheck ((\xs -> null xs || myMinimum1 xs == minimum xs) :: [Integer] -> Bool)

-- same as above using the myMaximumBy2 function
myMinimum2 :: Ord a => [a] -> a
myMinimum2 = myMinimumBy2 compare

test13_2 = quickCheck ((\xs -> null xs || myMinimum2 xs == minimum xs) :: [Integer] -> Bool)


----------------------------------------------------------------------------------------------
-- testing functions in main
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
  print $ "myAnd4 [] == True  -->  " ++ show (myAnd4 [] == True)
  print $ "myAnd4 [True, True, True] == True  -->  " ++ show (myAnd4 [True, True, True] == True)
  print $ "myAnd4 [True, False, True] == False  -->  " ++ show (myAnd4 [True, False, True] == False)
  print $ "myAnd5 [] == True  -->  " ++ show (myAnd5 [] == True)
  print $ "myAnd5 [True, True, True] == True  -->  " ++ show (myAnd5 [True, True, True] == True)
  print $ "myAnd5 [True, False, True] == False  -->  " ++ show (myAnd5 [True, False, True] == False)

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
  print $ "myOr4 [] == False  -->  " ++ show (myOr4 [] == False)
  print $ "myOr4 [False, False, True] == True  -->  " ++ show (myOr4 [False, False, True] == True)
  print $ "myOr4 [False, False, False] == False  -->  " ++ show (myOr4 [False, False, False] == False)

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
  print $ "myAny4 even [] == False  -->  " ++ show (myAny4 even [] == False)
  print $ "myAny4 even [1, 3, 5] == False  -->  " ++ show (myAny4 even [1, 3, 5] == False)
  print $ "myAny4 even [1, 3, 6] == True  -->  " ++ show (myAny4 even [1, 3, 6] == True)

  print $ "----- 3. myElem ----------"
  print $ "myElem1 6 [] == False  -->  " ++ show (myElem1 6 [] == False)
  print $ "myElem1 6 [1, 3, 5] == False  -->  " ++ show (myElem1 6 [1, 3, 5] == False)
  print $ "myElem1 6 [1, 3, 6] == True  -->  " ++ show (myElem1 6 [1, 3, 6] == True)
  print $ "myElem2 6 [] == False  -->  " ++ show (myElem2 6 [] == False)
  print $ "myElem2 6 [1, 3, 5] == False  -->  " ++ show (myElem2 6 [1, 3, 5] == False)
  print $ "myElem2 6 [1, 3, 6] == True  -->  " ++ show (myElem2 6 [1, 3, 6] == True)
  print $ "myElem3 6 [] == False  -->  " ++ show (myElem3 6 [] == False)
  print $ "myElem3 6 [1, 3, 5] == False  -->  " ++ show (myElem3 6 [1, 3, 5] == False)
  print $ "myElem3 6 [1, 3, 6] == True  -->  " ++ show (myElem3 6 [1, 3, 6] == True)

  print $ "----- 4. myReverse ----------"
  -- print $ "myReverse1 [] == []  -->  " ++ show (myReverse1 [] == []) -- does not compile ????
  print $ "reverse (myReverse1 [1, 3, 5]) == [1, 3, 5]  -->  " ++ show (reverse (myReverse1 [1, 3, 5]) == [1, 3, 5])
  print $ "reverse (myReverse1 \"blah\") == \"blah\"  -->  " ++ show (reverse (myReverse1 "blah") == "blah")
  -- print $ "myReverse2 [] == []  -->  " ++ show (myReverse2 [] == []) -- does not compile ????
  print $ "reverse (myReverse2 [1, 3, 5]) == [1, 3, 5]  -->  " ++ show (reverse (myReverse2 [1, 3, 5]) == [1, 3, 5])
  print $ "reverse (myReverse2 \"blah\") == \"blah\"  -->  " ++ show (reverse (myReverse2 "blah") == "blah")
  -- print $ "myReverse3 [] == []  -->  " ++ show (myReverse3 [] == []) -- does not compile ????
  print $ "reverse (myReverse3 [1, 3, 5]) == [1, 3, 5]  -->  " ++ show (reverse (myReverse3 [1, 3, 5]) == [1, 3, 5])
  print $ "reverse (myReverse3 \"blah\") == \"blah\"  -->  " ++ show (reverse (myReverse3 "blah") == "blah")

  print $ "----- 5. myMap ----------"
  print $ "myMap1 (^2) [] == []  -->  " ++ show (myMap1 (^2) [] == [])
  print $ "myMap1 (^2) [1..4] == [1,4,9,16]  -->  " ++ show (myMap1 (^2) [1..4] == [1,4,9,16])
  print $ "myMap1 (toUpper) \"some string\" == \"SOME STRING\"  -->  " ++ show (myMap1 (toUpper) "some string" == "SOME STRING")
  print $ "myMap2 (^2) [] == []  -->  " ++ show (myMap2 (^2) [] == [])
  print $ "myMap2 (^2) [1..4] == [1,4,9,16]  -->  " ++ show (myMap2 (^2) [1..4] == [1,4,9,16])
  print $ "myMap2 (toUpper) \"some string\" == \"SOME STRING\"  -->  " ++ show (myMap2 (toUpper) "some string" == "SOME STRING")

  print $ "----- 6. myFilter ----------"
  print $ "myFilter1 even [] == []  -->  " ++ show (myFilter1 even [] == [])
  print $ "myFilter1 even [1..6] == [2,4,6]  -->  " ++ show (myFilter1 even [1..6] == [2,4,6])
  print $ "myFilter1 isAlpha \"so5m,e-s(tr3ing\" == \"somestring\"  -->  " ++ show (myFilter1 (isAlpha) "so5m,e-s(tr3ing" == "somestring")
  print $ "myFilter2 even [] == []  -->  " ++ show (myFilter2 even [] == [])
  print $ "myFilter2 even [1..6] == [2,4,6]  -->  " ++ show (myFilter2 even [1..6] == [2,4,6])
  print $ "myFilter2 isAlpha \"so5m,e-s(tr3ing\" == \"somestring\"  -->  " ++ show (myFilter2 (isAlpha) "so5m,e-s(tr3ing" == "somestring")

  print $ "----- 7. squish ----------"
  -- print $ "squish1 [] == []  -->  " ++ show (squish1 [] == []) -- does not compile ????
  print $ "squish1 [\"abc\"] == \"abc\"  -->  " ++ show (squish1 ["abc"] == "abc")
  print $ "squish1 [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squish1 ["abc", " mno ", "xyz"] == "abc mno xyz")
  -- print $ "squish2 [] == []  -->  " ++ show (squish2 [] == []) -- does not compile ????
  print $ "squish2 [\"abc\"] == \"abc\"  -->  " ++ show (squish2 ["abc"] == "abc")
  print $ "squish2 [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squish2 ["abc", " mno ", "xyz"] == "abc mno xyz")

  print $ "----- 8. squishMap ----------"
  print $ "squishMap1 (\\x -> [1, x, 3]) [] == []  -->  " ++ show (squishMap1 (\x -> [1, x, 3]) [] == [])
  print $ "squishMap1 (\\x -> [1, x, 3]) [2] == [1,2,3]  -->  " ++ show (squishMap1 (\x -> [1, x, 3]) [2] == [1,2,3])
  print $ "squishMap1 (\\x -> \"WO \"++[x]++\" HOO \") \"123\" [2] == \"WO 1 HOO WO 2 HOO WO 3 HOO \"  -->  " ++ show (squishMap1 (\x -> "WO "++[x]++" HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO ")
  print $ "squishMap2 (\\x -> [1, x, 3]) [] == []  -->  " ++ show (squishMap2 (\x -> [1, x, 3]) [] == [])
  print $ "squishMap2 (\\x -> [1, x, 3]) [2] == [1,2,3]  -->  " ++ show (squishMap2 (\x -> [1, x, 3]) [2] == [1,2,3])
  print $ "squishMap2 (\\x -> \"WO \"++[x]++\" HOO \") \"123\" [2] == \"WO 1 HOO WO 2 HOO WO 3 HOO \"  -->  " ++ show (squishMap2 (\x -> "WO "++[x]++" HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO ")
  print $ "squishMap3 (\\x -> [1, x, 3]) [] == []  -->  " ++ show (squishMap3 (\x -> [1, x, 3]) [] == [])
  print $ "squishMap3 (\\x -> [1, x, 3]) [2] == [1,2,3]  -->  " ++ show (squishMap3 (\x -> [1, x, 3]) [2] == [1,2,3])
  print $ "squishMap3 (\\x -> \"WO \"++[x]++\" HOO \") \"123\" [2] == \"WO 1 HOO WO 2 HOO WO 3 HOO \"  -->  " ++ show (squishMap3 (\x -> "WO "++[x]++" HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO ")

  print $ "----- 9. squishAgain ----------"
  -- print $ "squishAgain1 [] == []  -->  " ++ show (squishAgain1 [] == []) -- does not compile ????
  print $ "squishAgain1 [\"abc\"] == \"abc\"  -->  " ++ show (squishAgain1 ["abc"] == "abc")
  print $ "squishAgain1 [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squishAgain1 ["abc", " mno ", "xyz"] == "abc mno xyz")
  -- print $ "squishAgain2 [] == []  -->  " ++ show (squishAgain2 [] == []) -- does not compile ????
  print $ "squishAgain2 [\"abc\"] == \"abc\"  -->  " ++ show (squishAgain2 ["abc"] == "abc")
  print $ "squishAgain2 [\"abc\", \" mno \", \"xyz\"] == \"abc mno xyz\"  -->  " ++ show (squishAgain2 ["abc", " mno ", "xyz"] == "abc mno xyz")

  print $ "----- 10. myMaximumBy ----------"
  print $ "myMaximumBy1 compare [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximumBy1 compare [1, 53, 9001, 10] == 9001)
  print $ "myMaximumBy2 compare [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximumBy2 compare [1, 53, 9001, 10] == 9001)

  print $ "-----11. myMinimumBy ----------"
  print $ "myMinimumBy1 compare [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimumBy1 compare [1, 53, 9001, 10] == 1)
  print $ "myMinimumBy2 compare [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimumBy2 compare [1, 53, 9001, 10] == 1)

  print $ "----- 12. myMaximum ----------"
  print $ "myMaximum1 [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximum1 [1, 53, 9001, 10] == 9001)
  print $ "myMaximum2 [1, 53, 9001, 10] == 9001  -->  " ++ show (myMaximum2 [1, 53, 9001, 10] == 9001)

  print $ "----- 13. myMinimum ----------"
  print $ "myMinimum1 [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimum2 [1, 53, 9001, 10] == 1)
  print $ "myMinimum2 [1, 53, 9001, 10] == 1  -->  " ++ show (myMinimum2 [1, 53, 9001, 10] == 1)

  print $ "----- Performing tests with QuickCheck ----------"
  test0_1
  test0_2
  test0_3
  test0_4
  test0_5
  test1_1
  test1_2
  test1_3
  test1_4
  test2_1
  test2_2
  test2_3
  test2_4
  test3_1
  test3_2
  test3_3
  test4_1
  test4_2
  test4_3
  test5_1
  test5_2
  test6_1
  test6_2
  test7_1
  test7_2
  test8_1a
  test8_1b
  test8_2a
  test8_2b
  test8_3a
  test8_3b
  test9_1
  test9_2
  test9_3
  test10_1
  test10_2
  test11_1
  test11_2
  test12_1
  test12_2
  test13_1
  test13_2
  