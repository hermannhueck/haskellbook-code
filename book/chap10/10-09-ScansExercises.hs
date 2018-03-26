-- 10-09-ScansExercises.hs
--
-- 10.9 Scans, page 376
-- Scans Exercises, page 379
--
module ScansExercises where


-- Fibonacci function from chapter
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x


-- 1. Modify your fibs function to only return the first 20 Fibonacci numbers.

fibs20 = take 20 fibs


-- 2. Modify fibs to return the Fibonacci numbers that are less than 100.

fibsUpTo100 = takeWhile (<100) fibs


-- 3. Try to write the factorial function from Recursion as a scan.
-- You’ll want scanl again, and your start value will be 1. Warning:
-- this will also generate an infinite list, so you may want to pass
-- it through a take function or similar.

fac = scanl (*) 1 [1..]
facN x = fac !! x

fac20 = take 20 fac
fac10 = take 10 fac

facUpTo10000 = takeWhile (<10000) fac
