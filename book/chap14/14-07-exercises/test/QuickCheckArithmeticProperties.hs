module QuickCheckArithmeticProperties
    (checkArithmeticProperties
    ) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper, toLower)
 

-- 1.

-- for a function
half :: Fractional a => a -> a
half x = x / 2

-- this property should hold
halfIdentity :: Double -> Double
halfIdentity = (*2) . half


-- 2.

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go y (Nothing, isOrdered) = (Just y, isOrdered)
        go y (Just x, isOrdered) = (Just y, x >= y)

-- 3. assotiativity and commutativity of addition

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

-- 4. assotiativity and commutativity of multiplication

multAssociative :: Integer -> Integer -> Integer -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Integer -> Integer -> Bool
multCommutative x y = x * y == y * x

-- 5. We mentioned in one of the  rst chapters that there are some laws involving
-- the relationship of quot and rem and div and mod. Write QuickCheck tests to prove them.

quotRemLaw :: Property
quotRemLaw =
    forAll nonZeroInt $ \x -> 
    forAll nonZeroInt $ \y -> 
    (quot x y)*y + (rem x y) == x

divModLaw :: Property
divModLaw =
    forAll nonZeroInt $ \x -> 
    forAll nonZeroInt $ \y -> 
    (div x y) * y + (mod x y) == x
  
nonZeroInt :: Gen Int
nonZeroInt = (arbitrary :: Gen Int) `suchThat` nonZero
  where 
    nonZero = (/=) 0

-- 6. contradict assotiativity and commutativity of power

prop_powerAssociative :: Property
prop_powerAssociative = forAll
  (arbitrary :: Gen Integer)
  powerAssociative

powerAssociative :: Integer -> Integer -> Integer -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_powerCommutative :: Property
prop_powerCommutative = forAll
  (arbitrary :: Gen Integer)
  powerCommutative

powerCommutative :: Integer -> Integer -> Bool
powerCommutative x y = x ^ y == y ^ x

-- 7. Test that reversing a list twice is the same as the identity of the list

listReversedTwice :: Eq a => [a] -> Bool
listReversedTwice xs = reverse (reverse xs) == id xs

-- 8. ($) operator and function composition

dollarOperatorLaw :: Eq b => (a -> b) -> a -> Bool
dollarOperatorLaw f x = ($) f x == f x

functionCompositionLaw :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
functionCompositionLaw f g x = (f . g) x == f (g x)

-- 9. variants of foldr

foldrConsEqualsPlusPlus :: Eq a => [a] -> [a] -> Bool
foldrConsEqualsPlusPlus xs ys = foldr (:) xs ys == (++) xs ys

foldrFlippedConsEqualsPlusPlus :: Eq a => [a] -> [a] -> Bool
foldrFlippedConsEqualsPlusPlus xs ys = foldr (:) xs ys == flip (++) xs ys

foldrPlusPlusEqualsConcat :: Eq a => [[a]] -> Bool
foldrPlusPlusEqualsConcat xs = foldr (++) [] xs == concat xs

-- 10. Hm. Is that so?

lengthOfTakeNEqualsN :: Int -> [a] -> Bool
lengthOfTakeNEqualsN n xs = length (take n xs) == n

-- 11. Finally, this is a fun one. You may remember we had you compose read and show one time
-- to complete a “round trip.” Well, now you can test that it works:

readShowXReturnsXInt :: Int -> Bool
readShowXReturnsXInt x = (read (show x)) == x

readShowXReturnsXDouble :: Double -> Bool
readShowXReturnsXDouble x = (read (show x)) == x

-- Failure of: squareIdentity = square . sqrt

-- for a function
square :: Num a => a -> a
square x = x * x

-- why does this property not hold? Examine the type of sqrt.
checkSquareIdentity :: (Eq a, Floating a) => a -> Bool
checkSquareIdentity x = (square . sqrt) x == x

-- Idempotency of 'capitalizeWord' and 'sort'

twice f = f . f
fourTimes = twice . twice

-- 1.

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : map toLower xs

idempotencyOfCapitalizeWord :: String -> Bool
idempotencyOfCapitalizeWord word =
    (capitalizeWord word == twice capitalizeWord word)
    && (capitalizeWord word == fourTimes capitalizeWord word)

-- 2.
idempotencyOfSortList :: Ord a => [a] -> Bool
idempotencyOfSortList list =
    (sort list == twice sort list)
    && (sort list == fourTimes sort list)



-- testing the above properties

checkArithmeticProperties :: IO ()
checkArithmeticProperties = hspec $ do

    describe "Testing arithmetic properties ..." $ do
        it "1. 2 times the half of a value equals the value." $ do
            property $ \x -> halfIdentity x == (x :: Double)
        it "2. In a sorted list every 2 elements are in order." $ do
            property ((listOrdered . sort) :: [Integer] -> Bool)
        it "3a. Addition of Integer is assotiative." $ do
            property plusAssociative
        it "3b. Addition of Integer is commutative." $ do
            property plusCommutative
        it "4a. Multiplication of Integer is assotiative." $ do
            property multAssociative
        it "4b. Multiplication of Integer is commutative." $ do
            property multCommutative
        it "5a. The law for quot and rem holds: (quot x y)*y + (rem x y) == x" $ do
            property quotRemLaw
        it "5b. The law for div and mod holds:  (div x y)*y  + (mod x y) == x" $ do
            property divModLaw
        it "6a. Power operation for Integer is not assotiative.     -- using boolean expression" $ do
            property $ expectFailure powerAssociative
        it "6b. Power operation for Integer is not assotiative.     -- using Property" $ do
            property $ expectFailure prop_powerAssociative
        it "6c. Power operation for Integer is not commutative.     -- using boolean expression" $ do
            property $ expectFailure powerCommutative
        it "6d. Power operation for Integer is not commutative.     -- using Property" $ do
            property $ expectFailure prop_powerCommutative
        it "7. Reversing a list twice is the same as the identity of the list." $ do
            property (listReversedTwice :: [Integer] -> Bool)
        it "8a. Operator ($): f $ x == f x" $ do
            property ((dollarOperatorLaw (+1)) :: Int -> Bool)
        it "8b. Fuction Composition ($): (f . g) x == f (g x)" $ do
            property ((functionCompositionLaw (+1) (*2)) :: Int -> Bool)
        it "9a. Should fail: foldr (:) == (++)" $ do
            property $ expectFailure (foldrConsEqualsPlusPlus :: [Int] -> [Int] -> Bool)
        it "9b. Should succeed: foldr (:) == flip (++)" $ do
            property (foldrFlippedConsEqualsPlusPlus :: [Int] -> [Int] -> Bool)
        it "9c. Should succeed: foldr (:) [] == concat" $ do
            property (foldrPlusPlusEqualsConcat :: [[Int]] -> Bool)
        it "10. Should fail: length (take n xs) == n" $ do
            property $ expectFailure (lengthOfTakeNEqualsN :: Int -> [Int] -> Bool)
        it "11a. Should succeed for Int: (read (show x)) == x" $ do
            property readShowXReturnsXInt
        it "11b. Should succeed for Double: (read (show x)) == x" $ do
            property readShowXReturnsXDouble
        it "Failure: Square identity does not hold: (sqare . sqrt) x == x" $ do
            property $ expectFailure (checkSquareIdentity :: Double -> Bool)
        it "Idempotency 1: capitalizeWord should be idempotent" $ do
            property idempotencyOfCapitalizeWord
        it "Idempotency 2: sorting a list should be idempotent" $ do
            property (idempotencyOfSortList :: [Integer] -> Bool)

{-

The test sequence produces the following output:

Testing arithmetic properties ...
  1. 2 times the half of a value equals the value.
  2. In a sorted list every 2 elements are in order.
  3a. Addition of Integer is assotiative.
  3b. Addition of Integer is commutative.
  4a. Multiplication of Integer is assotiative.
  4b. Multiplication of Integer is commutative.
  5a. The law for quot and rem holds: (quot x y)*y + (rem x y) == x
  5b. The law for div and mod holds:  (div x y)*y  + (mod x y) == x
  6a. Power operation for Integer is not assotiative.
  6b. Power operation for Integer is not commutative.
  7. Reversing a list twice is the same as the identity of the list.
  8a. Operator ($): f $ x == f x
  8b. Fuction Composition ($): (f . g) x == f (g x)
  9a. Should fail: foldr (:) == (++)
  9b. Should succeed: foldr (:) == flip (++)
  9c. Should succeed: foldr (:) [] == concat
  10. Should fail: length (take n xs) == n
  11a. Should succeed for Int: (read (show x)) == x
  11b. Should succeed for Double: (read (show x)) == x
  Failure: Square identity does not hold: (sqare . sqrt) x == x
  Idempotency 1: capitalizeWord should be idempotent
  Idempotency 2: sorting a list should be idempotent

Finished in 0.1210 seconds
22 examples, 0 failures
        
-}