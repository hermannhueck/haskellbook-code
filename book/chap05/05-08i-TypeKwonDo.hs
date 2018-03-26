-- 05-08i-TypeKwonDo.hs
--
-- 5.8 Chapter Exercises, page 147
-- Type-Kwon-Do, page 155
--
module TypeKwonDo where

-- 0.
data Woot

data Blah

f0 :: Woot -> Blah
f0 = undefined

g0 :: (Blah, Woot) -> (Blah, Blah)
g0 (b, w) = (b, b)

-- 1.
f1 :: Int -> String
f1 = undefined

g1 :: String -> Char
g1 = undefined

h1 :: Int -> Char
h1 x = g1 $ f1 x

-- 2.
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- 3.
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

xform' :: (X, Y) -> (Z, Z)
xform' (x, y) = (yz y, xz x)

xform'' :: (X, Y) -> (Z, Z)
xform'' (x, y) = (xz x, xz x)

xform''' :: (X, Y) -> (Z, Z)
xform''' (x, y) = (yz y, yz y)

-- 4.
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge x2y y2wz x = fst $ y2wz $ x2y x
