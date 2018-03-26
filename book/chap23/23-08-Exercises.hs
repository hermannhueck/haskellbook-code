-- 23-08-Exercises.hs
--
-- 23.8 Chapter Exercises, page 909
--
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Test.QuickCheck
import Test.Hspec

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> (f $ fst $ g s, s)
  -- or:         = Moi $ \s -> ((f . fst . g) s, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> ((fst $ f s) (fst $ g s), s)

instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi f >>= g =
    Moi $ \s ->
      let (a, s') = f s
      in runMoi (g a) s'
  -- or:      = Moi $ \s -> (runMoi . g . fst . f) s (snd . f $ s)

------------------------------------------------------------------------
-- Write the following functions. You'll want to use your own State type
-- for which you've defined Functor, Applicative and Monad.

------------------------------------------------------------------------
-- 1. Construct a State where the state is also the value you return.
get :: Moi s s
get = Moi $ \s -> (s, s)

------------------------------------------------------------------------
-- 2. Construct a State where the resulting state is the argument provided
--    and the value is defaulted to unit.
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

------------------------------------------------------------------------
-- 3. Run the State with s and get the state that results.
exec :: Moi s a -> s -> s
exec (Moi sa) s = let (_, s') = sa s
                  in s'

exec' :: Moi s a -> s -> s
exec' (Moi sa) s = snd $ sa s

exec'' :: Moi s a -> s -> s
exec'' (Moi sa) = snd . sa

exec''' :: Moi s a -> s -> s
exec''' (Moi sa) s = snd $ runMoi (Moi $ \_ -> sa s) s

exec'''' :: Moi s a -> s -> s
exec'''' msa s = snd $ runMoi msa s

exec''''' :: Moi s a -> s -> s
exec''''' = (snd .) . runMoi

------------------------------------------------------------------------
-- 4. Run the State with s and get the value that results.
eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a, _) = sa s
                  in a

eval' :: Moi s a -> s -> a
eval' (Moi sa) s = fst $ sa s

eval'' :: Moi s a -> s -> a
eval'' (Moi sa) = fst . sa

eval''' :: Moi s a -> s -> a
eval''' (Moi sa) s = fst $ runMoi (Moi $ \_ -> sa s) s

eval'''' :: Moi s a -> s -> a
eval'''' msa s = fst $ runMoi msa s

eval''''' :: Moi s a -> s -> a
eval''''' = (fst .) . runMoi

------------------------------------------------------------------------
-- 5. Write a function that applies a function to create a new state.
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s) 

modify' :: (s -> s) -> Moi s ()
modify' f = f <$> get >>= put

------------------------------------------------------------------------
-- Tests in main
main :: IO ()
main = do
  putStrLn "----- Visual Control ----------"
  putStrLn $ "runMoi get \"curryIsAmaze\"               -->  " ++ show (runMoi get "curryIsAmaze")
  putStrLn $ "runMoi (put \"blah\") \"woot\"              -->  " ++ show (runMoi (put "blah") "woot")
  putStrLn $ "runMoi (put' \"blah\") \"woot\"             -->  " ++ show (runMoi (put "blah") "woot")
  putStrLn $ "exec (put \"wilma\") \"daphne\"             -->  " ++ show (exec (put "wilma") "daphne")
  putStrLn $ "exec' (put \"wilma\") \"daphne\"            -->  " ++ show (exec' (put "wilma") "daphne")
  putStrLn $ "exec'' (put \"wilma\") \"daphne\"           -->  " ++ show (exec'' (put "wilma") "daphne")
  putStrLn $ "exec''' (put \"wilma\") \"daphne\"          -->  " ++ show (exec''' (put "wilma") "daphne")
  putStrLn $ "exec'''' (put \"wilma\") \"daphne\"         -->  " ++ show (exec'''' (put "wilma") "daphne")
  putStrLn $ "exec''''' (put \"wilma\") \"daphne\"        -->  " ++ show (exec''''' (put "wilma") "daphne")
  putStrLn $ "exec get \"scooby papu\"                  -->  " ++ show (exec get "scooby papu")
  putStrLn $ "exec' get \"scooby papu\"                 -->  " ++ show (exec' get "scooby papu")
  putStrLn $ "exec'' get \"scooby papu\"                -->  " ++ show (exec'' get "scooby papu")
  putStrLn $ "exec''' get \"scooby papu\"               -->  " ++ show (exec''' get "scooby papu")
  putStrLn $ "exec'''' get \"scooby papu\"              -->  " ++ show (exec'''' get "scooby papu")
  putStrLn $ "exec''''' get \"scooby papu\"             -->  " ++ show (exec''''' get "scooby papu")
  putStrLn $ "eval get \"bunnicula\"                    -->  " ++ show (eval get "bunnicula")
  putStrLn $ "eval' get \"bunnicula\"                   -->  " ++ show (eval' get "bunnicula")
  putStrLn $ "eval'' get \"bunnicula\"                  -->  " ++ show (eval'' get "bunnicula")
  putStrLn $ "eval''' get \"bunnicula\"                 -->  " ++ show (eval''' get "bunnicula")
  putStrLn $ "eval'''' get \"bunnicula\"                -->  " ++ show (eval'''' get "bunnicula")
  putStrLn $ "eval''''' get \"bunnicula\"               -->  " ++ show (eval''''' get "bunnicula")
  putStrLn $ "eval get \"stake a bunny\"                -->  " ++ show (eval get "stake a bunny")
  putStrLn $ "eval' get \"stake a bunny\"               -->  " ++ show (eval' get "stake a bunny")
  putStrLn $ "eval'' get \"stake a bunny\"              -->  " ++ show (eval'' get "stake a bunny")
  putStrLn $ "eval''' get \"stake a bunny\"             -->  " ++ show (eval''' get "stake a bunny")
  putStrLn $ "eval'''' get \"stake a bunny\"            -->  " ++ show (eval'''' get "stake a bunny")
  putStrLn $ "eval''''' get \"stake a bunny\"           -->  " ++ show (eval''''' get "stake a bunny")
  putStrLn $ "eval (put \"blah\") \"woot\"                -->  " ++ show (eval (put "blah") "woot")
  putStrLn $ "eval' (put \"blah\") \"woot\"               -->  " ++ show (eval' (put "blah") "woot")
  putStrLn $ "eval'' (put \"blah\") \"woot\"              -->  " ++ show (eval'' (put "blah") "woot")
  putStrLn $ "eval''' (put \"blah\") \"woot\"             -->  " ++ show (eval''' (put "blah") "woot")
  putStrLn $ "eval'''' (put \"blah\") \"woot\"            -->  " ++ show (eval'''' (put "blah") "woot")
  putStrLn $ "eval''''' (put \"blah\") \"woot\"           -->  " ++ show (eval''''' (put "blah") "woot")
  putStrLn $ "runMoi (modify (+1)) 0                  -->  " ++ show (runMoi (modify (+1)) 0)
  putStrLn $ "runMoi (modify' (+1)) 0                 -->  " ++ show (runMoi (modify' (+1)) 0)
  putStrLn $ "runMoi (modify (+1) >> modify (+1)) 0   -->  " ++ show (runMoi (modify (+1) >> modify (+1)) 0)
  putStrLn $ "runMoi (modify' (+1) >> modify' (+1)) 0 -->  " ++ show (runMoi (modify' (+1) >> modify' (+1)) 0)
  hspec $ do
    context "----- Property Tests with Hspec and QuickCheck" $ do
      it "\\xs -> runMoi get xs == (xs, xs)" $ do
        property ((\xs -> runMoi get xs == (xs, xs)) :: [Char] -> Bool)
      it "\\xs ys -> runMoi (put xs) ys == ((), xs)" $ do
        property ((\xs ys -> runMoi (put xs) ys == ((), xs)) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> runMoi (put' xs) ys == ((), xs)" $ do
        property ((\xs ys -> runMoi (put' xs) ys == ((), xs)) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec (put xs) ys == xs" $ do
        property ((\xs ys -> exec (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec' (put xs) ys == xs" $ do
        property ((\xs ys -> exec' (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec'' (put xs) ys == xs" $ do
        property ((\xs ys -> exec'' (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec''' (put xs) ys == xs" $ do
        property ((\xs ys -> exec''' (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec'''' (put xs) ys == xs" $ do
        property ((\xs ys -> exec'''' (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> exec''''' (put xs) ys == xs" $ do
        property ((\xs ys -> exec''''' (put xs) ys == xs) :: [Char] -> [Char] -> Bool)
      it "\\xs -> exec get xs == xs" $ do
        property ((\xs -> exec get xs == xs) :: [Char] -> Bool)
      it "\\xs -> exec' get xs == xs" $ do
        property ((\xs -> exec' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> exec'' get xs == xs" $ do
        property ((\xs -> exec'' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> exec''' get xs == xs" $ do
        property ((\xs -> exec''' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> exec'''' get xs == xs" $ do
        property ((\xs -> exec'''' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> exec''''' get xs == xs" $ do
        property ((\xs -> exec''''' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval get xs == xs" $ do
        property ((\xs -> eval get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval' get xs == xs" $ do
        property ((\xs -> eval' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval'' get xs == xs" $ do
        property ((\xs -> eval'' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval''' get xs == xs" $ do
        property ((\xs -> eval''' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval'''' get xs == xs" $ do
        property ((\xs -> eval'''' get xs == xs) :: [Char] -> Bool)
      it "\\xs -> eval''''' get xs == xs" $ do
        property ((\xs -> eval''''' get xs == xs) :: [Char] -> Bool)
      it "\\xs ys -> eval (put xs) ys == ()" $ do
        property ((\xs ys -> eval (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> eval' (put xs) ys == ()" $ do
        property ((\xs ys -> eval' (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> eval'' (put xs) ys == ()" $ do
        property ((\xs ys -> eval'' (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> eval''' (put xs) ys == ()" $ do
        property ((\xs ys -> eval''' (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> eval'''' (put xs) ys == ()" $ do
        property ((\xs ys -> eval'''' (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\xs ys -> eval''''' (put xs) ys == ()" $ do
        property ((\xs ys -> eval''''' (put xs) ys == ()) :: [Char] -> [Char] -> Bool)
      it "\\x y -> runMoi (modify (+x)) y == ((), x + y)" $ do
        property ((\x y -> runMoi (modify (+x)) y == ((), x + y)) :: Integer -> Integer -> Bool)
      it "\\x y -> runMoi (modify' (+x)) y == ((), x + y)" $ do
        property ((\x y -> runMoi (modify' (+x)) y == ((), x + y)) :: Integer -> Integer -> Bool)
      it "\\x y z -> runMoi (modify (+x) >> modify (+y)) z == ((), x + y + z)" $ do
        property ((\x y z -> runMoi (modify (+x) >> modify (+y)) z == ((), x + y + z)) :: Integer -> Integer -> Integer -> Bool)
      it "\\x y z -> runMoi (modify' (+x) >> modify' (+y)) z == ((), x + y + z)" $ do
        property ((\x y z -> runMoi (modify' (+x) >> modify' (+y)) z == ((), x + y + z)) :: Integer -> Integer -> Integer -> Bool)
