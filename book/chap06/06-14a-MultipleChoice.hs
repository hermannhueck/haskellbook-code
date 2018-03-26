-- 06-14a-MultipleChoice.hs
--
-- 6.14 Chapter Exercises, page 206
-- Multiple Choice, page 206
--
module MultipleChoice where

{-

1. The Eq class
   a) includes all types in Haskell
   b) is the same as the Ord class
   c) makes equality tests possible
   d) only includes numeric types
Answer: c)

2. The typeclass Ord
   a) allows any two values to be compared
   b) is a subclass of Eq
   c) is a superclass of Eq
   d) has no instance for Bool
Answer: b)

3. Suppose the typeclass Ord has an operator >.
   What is the type of >?
   a) Ord a => a -> a -> Bool
   b) Ord a => Int -> Bool
   c) Ord a => a -> Char
   d) Ord a => Char -> [Char]
Answer: a)

4. In x = divMod 16 12
   a) the type of x is Integer
   b) the value of x is undecidable
   c) the type of x is a tuple
   d) x is equal to 12 / 16
Answer: c)

5. The typeclass Integral includes
   a) Int and Integer numbers
   b) integral, real and fractional numbers
   c) Schrodinger's cat
   d) only positive numbers
Answer: a)

-}