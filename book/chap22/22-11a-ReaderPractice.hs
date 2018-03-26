-- 22-11-ReaderPractice.hs
--
-- 22.11 Chapter Exercises, page 885
--
module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- type of lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- tuple xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- tuple ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'        -- same as:   x3 n = (z' n, z' n)

-- type of uncurry
-- uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

-- type of fromMaybe
-- fromMaybe :: a -> Maybe a -> a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

assert =
     x1 == Just (6, 9)
  && x2 == Nothing
  && x3 3 == (Just 9, Just 9)
  && x3 4 == (Nothing, Nothing)

main = do
  if assert /= True
    then putStrLn "error: assertion is False"
    else return ()
  putStrLn "--- outputs: "
  print $ "(sequenceA [Just 3, Just 2, Just 1]) ==> " ++ show (sequenceA [Just 3, Just 2, Just 1])
  print $ "(sequenceA [x, y]) ==> " ++ show (sequenceA [x, y])
  print $ "(sequenceA [xs, ys]) ==> " ++ show (sequenceA [xs, ys])
  print $ "(summed <$> ((,) <$> xs <*> ys)) ==> " ++ show (summed <$> ((,) <$> xs <*> ys))
  print $ "(fmap summed ((,) <$> xs <*> zs)) ==> " ++ show (fmap summed ((,) <$> xs <*> zs))
  print $ "(bolt 7) ==> " ++ show (bolt 7)
  print $ "show (fmap bolt z) ==> " ++ show (fmap bolt z)
  print $ "(sequenceA [(>3), (<8), even] 7) ==> " ++ show (sequenceA [(>3), (<8), even] 7)
  print $ "(foldr (&&) True $ sequA $ fromMaybe 0 xs) ==> " ++ show (foldr (&&) True $ sequA $ fromMaybe 0 xs)        -- can also be expressed as ...
  print $ "(and <$> sequA <$> fromMaybe 0 $ xs) ==> " ++ show (and <$> sequA <$> fromMaybe 0 $ xs)
  print $ "(and . sequA . fromMaybe 0 $ xs) ==> " ++ show (and . sequA . fromMaybe 0 $ xs)
  print $ "(sequA $ fromMaybe 0 s') ==> " ++ show (sequA $ fromMaybe 0 s')                          -- can also be expressed as ...
  print $ "(sequA <$> fromMaybe 0 $ s') ==> " ++ show (sequA <$> fromMaybe 0 $ s')
  print $ "(sequA . fromMaybe 0 $ s') ==> " ++ show (sequA . fromMaybe 0 $ s')
  print $ "(bolt $ fromMaybe 0 ys) ==> " ++ show (bolt $ fromMaybe 0 ys)                           -- can also be expressed as ...
  print $ "(bolt <$> fromMaybe 0 $ ys) ==> " ++ show (bolt <$> fromMaybe 0 $ ys)
  print $ "(bolt . fromMaybe 0 $ ys) ==> " ++ show (bolt . fromMaybe 0 $ ys)
  putStrLn "--- assertions: "
  print $ sequenceA [Just 3, Just 2, Just 1] == Just [3, 2, 1]
  print $ sequenceA [x, y] == [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
  print $ sequenceA [xs, ys] == Just [6,9]
  print $ (summed <$> ((,) <$> xs <*> ys)) == Just 15
  print $ fmap summed ((,) <$> xs <*> zs) == Nothing
  print $ bolt 7 == True
  print $ fmap bolt z == [True,False,False]
  print $ sequenceA [(>3), (<8), even] 7 == [True,True,False]
  print $ (foldr (&&) True $ sequA $ fromMaybe 0 xs) == True
  print $ (and <$> sequA <$> fromMaybe 0 $ xs) == True
  print $ (and . sequA . fromMaybe 0 $ xs) == True
  print $ (sequA $ fromMaybe 0 s') == [True,False,False]
  print $ (sequA <$> fromMaybe 0 $ s') == [True,False,False]
  print $ (sequA . fromMaybe 0 $ s') == [True,False,False]
  print $ (bolt $ fromMaybe 0 ys) == False
  print $ (bolt <$> fromMaybe 0 $ ys) == False
  print $ (bolt . fromMaybe 0 $ ys) == False
