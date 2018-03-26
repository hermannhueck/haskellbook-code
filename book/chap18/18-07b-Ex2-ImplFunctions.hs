-- 18-07b-ImplFunctions.hs
--
-- 18.07 Chapter Exercises, page 788
-- Implement functions, page 789
--
-- Write the following functions using the methods provided by Monad and Functor.
-- Using stuff like identity and composition is fine, but it has to typecheck with types provided.
--
module ImplFunctions where

import Control.Monad (join, liftM2)
import Data.Monoid ((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----------------------------------------------------------------------------------------------
-- 1.
j :: Monad m => m (m a) -> m a
j = join

testJ = j [[1, 2], [], [3]] == [1,2,3]
  && j (Just (Just 1)) == Just 1
  && j (Just Nothing) == (Nothing :: Maybe Integer)
  && j Nothing == (Nothing :: Maybe Integer)

j' :: Monad m => m (m a) -> m a
j' mm = mm >>= id               -- even shorter: j' = (>>= id)

testJ' = j' [[1, 2], [], [3]] == [1,2,3]
  && j' (Just (Just 1)) == Just 1
  && j' (Just Nothing) == (Nothing :: Maybe Integer)
  && j' Nothing == (Nothing :: Maybe Integer)

j'' :: Monad m => m (m a) -> m a
j'' mma = do
  ma <- mma
  a <- ma
  return a

testJ'' = j'' [[1, 2], [], [3]] == [1,2,3]
  && j'' (Just (Just 1)) == Just 1
  && j'' (Just Nothing) == (Nothing :: Maybe Integer)
  && j'' Nothing == (Nothing :: Maybe Integer)

----------------------------------------------------------------------------------------------
-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

testL1 = l1 (+1) [1..3] == [2..4]
  && l1 (+1) (Just 1) == Just 2
  && l1 (+1) Nothing == Nothing
  
l1' :: Monad m => (a -> b) -> m a -> m b
l1' f ma = do
  a <- ma
  return $ f a

testL1' = l1' (+1) [1..3] == [2..4]
  && l1' (+1) (Just 1) == Just 2
  && l1' (+1) Nothing == Nothing
  
----------------------------------------------------------------------------------------------
-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb                            -- using <$> and <*>

testL2 = l2 (+) [1..3] [1..3] == [2,3,4,3,4,5,4,5,6]
  && l2 (+) (Just 1) (Just 2) == Just 3
  && l2 (+) (Just 1) Nothing == Nothing
  && l2 (+) Nothing (Just 2) == Nothing
  && l2 (+) Nothing Nothing == Nothing
  
l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = ma >>= (\a -> (f a) <$> mb)               -- using fmap and bind

testL2' = l2' (+) [1..3] [1..3] == [2,3,4,3,4,5,4,5,6]
  && l2' (+) (Just 1) (Just 2) == Just 3
  && l2' (+) (Just 1) Nothing == Nothing
  && l2' (+) Nothing (Just 2) == Nothing
  && l2' (+) Nothing Nothing == Nothing

l2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2'' f ma mb = do                                       -- using do syntax
  a <- ma
  b <- mb
  return $ f a b

testL2'' = l2'' (+) [1..3] [1..3] == [2,3,4,3,4,5,4,5,6]
  && l2'' (+) (Just 1) (Just 2) == Just 3
  && l2'' (+) (Just 1) Nothing == Nothing
  && l2'' (+) Nothing (Just 2) == Nothing
  && l2'' (+) Nothing Nothing == Nothing

l2''' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2''' = liftM2

testL2''' = l2''' (+) [1..3] [1..3] == [2,3,4,3,4,5,4,5,6]
  && l2''' (+) (Just 1) (Just 2) == Just 3
  && l2''' (+) (Just 1) Nothing == Nothing
  && l2''' (+) Nothing (Just 2) == Nothing
  && l2''' (+) Nothing Nothing == Nothing

----------------------------------------------------------------------------------------------
-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

testA = a [1..3] [(+1), (*2)] == [2,3,4,2,4,6]
  && a (Just 2) (Just (+1)) == Just 3
  && a (Just 2) Nothing == (Nothing :: Maybe Integer)
  && a Nothing (Just (+1)) == Nothing
  && a Nothing Nothing == (Nothing :: Maybe Integer)
  
a' :: Monad m => m a -> m (a -> b) -> m b
a' ma mf = mf >>= (\f -> f <$> ma)                -- even shorter:  a' ma mf = mf >>= (<$> ma)

testA' = a' [1..3] [(+1), (*2)] == [2,3,4,2,4,6]
  && a' (Just 2) (Just (+1)) == Just 3
  && a' (Just 2) Nothing == (Nothing :: Maybe Integer)
  && a' Nothing (Just (+1)) == Nothing
  && a' Nothing Nothing == (Nothing :: Maybe Integer)
  
a'' :: Monad m => m a -> m (a -> b) -> m b
a'' ma mf = do
  f <- mf
  a <- ma
  return $ f a

testA'' = a'' [1..3] [(+1), (*2)] == [2,3,4,2,4,6]
  && a'' (Just 2) (Just (+1)) == Just 3
  && a'' (Just 2) Nothing == (Nothing :: Maybe Integer)
  && a'' Nothing (Just (+1)) == Nothing
  && a'' Nothing Nothing == (Nothing :: Maybe Integer)
  
a''' :: Monad m => m a -> m (a -> b) -> m b
a''' = flip (<*>)

testA''' = a''' [1..3] [(+1), (*2)] == [2,3,4,2,4,6]
  && a''' (Just 2) (Just (+1)) == Just 3
  && a''' (Just 2) Nothing == (Nothing :: Maybe Integer)
  && a''' Nothing (Just (+1)) == Nothing
  && a''' Nothing Nothing == (Nothing :: Maybe Integer)
  
----------------------------------------------------------------------------------------------
-- 5. You'll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

testMeh = meh [1..6] (\x -> Just (x + 1)) == Just [2..7]
  && meh [1..6] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Just [3,1,1,0,0,0]
  && meh [0..5] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Nothing
  && meh [(-2)..3] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Nothing
  
meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _ = return []
meh' (x:xs) f = f x >>= (\b -> ((:) b) <$> (meh xs f))

testMeh' = meh' [1..6] (\x -> Just (x + 1)) == Just [2..7]
  && meh' [1..6] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Just [3,1,1,0,0,0]
  && meh' [0..5] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Nothing
  && meh' [(-2)..3] (\x -> if x == 0 then Nothing else Just (3 `div` x)) == Nothing
  
----------------------------------------------------------------------------------------------
-- 6. Hint: reuse "meh"
flipType :: (Monad m) => [m a] -> m [a]
flipType list = meh list id

testFlipType = flipType [Just 1, Just 2, Just 3] == Just [1,2,3]
  && flipType [Nothing, Just 2, Just 3] == Nothing
  && flipType [Just 1, Nothing, Just 3] == Nothing
  && flipType [Nothing, Nothing, Nothing] == (Nothing :: Maybe [Integer])
  
flipType' :: (Monad m) => [m a] -> m [a]
flipType' = flip meh' id

testFlipType' = flipType' [Just 1, Just 2, Just 3] == Just [1,2,3]
  && flipType' [Nothing, Just 2, Just 3] == Nothing
  && flipType' [Just 1, Nothing, Just 3] == Nothing
  && flipType' [Nothing, Nothing, Nothing] == (Nothing :: Maybe [Integer])
  
----------------------------------------------------------------------------------------------
-- Invoking tests in main
main :: IO ()
main = do
  print $ "test j:          " ++ (show testJ)
  print $ "test j':         " ++ (show testJ')
  print $ "test j'':        " ++ (show testJ'')
  print $ "test l1:         " ++ (show testL1)
  print $ "test l1':        " ++ (show testL1')
  print $ "test l2:         " ++ (show testL2)
  print $ "test l2':        " ++ (show testL2')
  print $ "test l2'':       " ++ (show testL2'')
  print $ "test l2''':      " ++ (show testL2''')
  print $ "test a:          " ++ (show testA)
  print $ "test a':         " ++ (show testA')
  print $ "test a'':        " ++ (show testA'')
  print $ "test a''':       " ++ (show testA''')
  print $ "test meh:        " ++ (show testMeh)
  print $ "test meh':       " ++ (show testMeh')
  print $ "test flipType:   " ++ (show testFlipType)
  print $ "test flipType':  " ++ (show testFlipType')
  