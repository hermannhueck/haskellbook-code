-- 07-11b-WriteCode.hs
--
-- 7.11 Chapter Exercises, page 262
-- Let's write code, page 263
--
module WriteCode where

-- 1. The following function returns the tenth digit of an integral argument.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = x `div` 10
    d = xLast `mod` 10

-- a) First, rewrite it using divMod
-- tensDigit' :: Integral a => a -> a
tensDigit' = snd . dm10 . fst . dm10
  where dm10 x = divMod x 10

-- b) Does the dovMod version have the same type as the original version?
-- if the type signature is not specified explicitly, the type is
-- tensDigit' :: Integer -> Integer

-- c) change to get the hundreds digit
hunsD :: Integral a => a -> a
hunsD x =  d
  where
    xLast = x `div` 100
    d = xLast `mod` 10

hunsD' = snd . dm10 . fst . dm10 . fst . dm10
  where dm10 x = divMod x 10

-- 2. different versions of foldBool

-- using if-then-else
foldBool0 :: a -> a -> Bool -> a
foldBool0 x y b = if b then y else x

-- using case expression
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = case b of
    False -> x
    True -> y

-- using guard
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b = y
    | otherwise = x

-- using pattern matching
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

testFoldBool =
     foldBool0 'x' 'y' True == foldBool3 'x' 'y' True
  && foldBool0 'x' 'y' False == foldBool3 'x' 'y' False
  && foldBool1 'x' 'y' True == foldBool3 'x' 'y' True
  && foldBool1 'x' 'y' False == foldBool3 'x' 'y' False
  && foldBool2 'x' 'y' True == foldBool3 'x' 'y' True
  && foldBool2 'x' 'y' False == foldBool3 'x' 'y' False

-- 3. Implement g
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5. pointfree version of roundTrip
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- 6. result type may be different from input type
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read (show a)

main :: IO ()
main = do
  print (roundTrip 4)
  print (roundTripPF 4)
  print (roundTrip 4 :: Int)
  print (roundTripPF 4 :: Int)
  print (roundTrip 4 :: Float)
  print (roundTripPF 4 :: Float)
  print ((roundTrip2 4) :: Int)
  print ((roundTrip2 4) :: Integer)
  print ((roundTrip2 4) :: Float)
  print ((roundTrip2 4) :: Double)
