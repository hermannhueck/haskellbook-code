-- 05-08e-FunctionForType.hs
--
-- 5.8 Chapter Exercises, page 147
-- Given a type, write the function, page 152
--
module FunctionForType where

-- 0.
myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, (yToZ (xToY x)))

-- 1. There is only one function definition that typechecks and
-- doesn't go into an infinite loop when you run it.
i :: a -> a
i = \x -> x

-- 2. There is only one version that works.
c :: a -> b -> a
c = \x -> \_ -> x

-- 3. Given alpha equivalence are c'' and c (from above) the same thing.
c'' :: b -> a -> b
c'' = \x -> \_ -> x

-- 4. Only one version that works.
c' :: a -> b -> b
c' = \_ -> \y -> y

-- 5. Multiple possibilities
r :: [a] -> [a]
r = \xs -> xs

r' :: [a] -> [a]
r' = \xs -> reverse xs

r'' :: [a] -> [a]
r'' = \xs -> tail xs

-- 6. Only one version that will typecheck.
co :: (b -> c) -> (a -> b) -> a -> c
co = \f g x -> f $ g x

-- 7. One version will typecheck.
a :: (a -> c) -> a -> a
a = \_ x -> x

-- 8. One version will typecheck.
a' :: (a -> b) -> a -> b
a' = \f x -> f x
