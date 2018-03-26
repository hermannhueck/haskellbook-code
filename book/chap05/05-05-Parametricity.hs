-- 05-05-Parametricity.hs
--
-- 5.5 Polymorphism, page 137
-- Parametricity, page 140
--
module Parametricity where

-- 1.
myId :: a -> a
myId x = x

-- 2.
f :: a -> a -> a
f x y = x

f' :: a -> a -> a
f' x y = y

-- 3.
g :: a -> b -> b
g x y = y
-- one possible implementation
-- Behaviour doesn't change if the types of a and b change
