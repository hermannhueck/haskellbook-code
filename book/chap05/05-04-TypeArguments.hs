-- 05-04-TypeArguments.hs
--
-- 5.4 Currying, page 125
-- Exercises: Type  Arguments, page 134
--
module TypeArguments where

-- 1.
-- If the type of 'f' is 
-- a -> a -> a -> a
-- and the type of 'x' is Char
-- then the type of 'f x' is
--
-- a) Char -> Char -> Char
-- b) x -> x -> x -> x
-- c) a -> a -> a
-- d) a -> a -> a -> Bool
-- Answer: a)

-- 2.
-- If the type of 'g' is
-- a -> b -> c -> b
-- then the type of 'g 0 'c' "woot"' is
--
-- a) String
-- b) Char -> String
-- c) Int
-- d) Char
-- Answer: d)

-- 3.
-- If the type of 'h' is
-- (Num a, Num b) => a -> b -> b
-- then the type of 'h 1.0 2' is
--
-- a) Double
-- b) Integer
-- c) Integral b => b
-- d) Num b => b
-- Answer: d)

-- Note that because the type variables a and b are different,
-- the compiler must assume that the types could be different.

-- 4.
-- If the type of 'h' is
-- (Num a, Num b) => a -> b -> b
-- then the type of 'h 1 (5.5 :: Double)' is
--
-- a) Integer
-- b) Fractional b => b
-- c) Double
-- d) Num b => b
-- Answer: c)

-- 5.
-- If the type of 'jackal' is
-- (Ord a, Eq b) => a -> b -> a
-- then the type of
-- jackal "keyboard" "has the word jackal in it"
-- is
--
-- a) [Char]
-- b) Eq b => b
-- c) b -> [Char]
-- d) b
-- e) Eq b => b -> [Char]
-- Answer: a)

-- 6.
-- If the type of 'jackal' is
-- (Ord a, Eq b) => a -> b -> a
-- then the type of
-- jackal "keyboard"
-- is
--
-- a) b
-- b) Eq b => b
-- c) [Char]
-- d) b -> [Char]
-- e) Eq b => b -> [Char]
-- Answer: e)

-- 7.
-- If the type of 'kessel' is
-- (Ord a, Num b) => a -> b -> a
-- then the type of
-- kessel 1 2
-- is
--
-- a) Integer
-- b) Int
-- c) a
-- d) (Num a, Ord a) => a
-- e) Ord a => a
-- f) Num a => a
-- Answer: d)

-- 8.
-- If the type of 'kessel' is
-- (Ord a, Num b) => a -> b -> a
-- then the type of
-- kessel 1 (2 :: Integer)
-- is
--
-- a) (Num a, Ord a) => a
-- b) Int
-- c) a
-- e) Ord a => a
-- f) Num a => a
-- d) Integer
-- Answer: a)

-- 9.
-- If the type of 'kessel' is
-- (Ord a, Num b) => a -> b -> a
-- then the type of
-- kessel (1 :: Integer) 2
-- is
--
-- a) Num a => a
-- b) Ord a => a
-- c) Integer
-- d) (Num a, Ord a) => a
-- e) a
-- Answer: a)