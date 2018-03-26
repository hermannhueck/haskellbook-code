-- 22-02b-WarmingUp.hs
--
-- 22.2 A new beginning, page 861
-- Warming Up, page 867
--

module WarmingUp where

import Data.Char
import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Test.QuickCheck

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- Two simple functions of the same type, taking the same type
-- of input. We could compose them using (.) or fmap.

-- function composition
composed :: [Char] -> [Char]
composed = cap . rev

-- functorial context
fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

-- applicative context
tupled1 :: [Char] -> ([Char], [Char])
tupled1 = (,) <$> cap <*> rev

-- applicative context: liftA2
tupled2 :: [Char] -> ([Char], [Char])
tupled2 = liftA2 (,) cap rev

-- monadic context: liftM2
tupled3 :: [Char] -> ([Char], [Char])
tupled3 = liftM2 (,) cap rev

-- monadic context: do syntax
tupled4 :: [Char] -> ([Char], [Char])
tupled4 = do
  capped <- cap
  reved <- rev
  return (capped, reved)

-- monadic context: bind
tupled5 :: [Char] -> ([Char], [Char])
tupled5 = cap >>= \capped ->
          rev >>= \reved ->
          return (capped, reved)

-- monadic context: flipped bind
tupled6 :: [Char] -> ([Char], [Char])
tupled6 = (\capped ->
            (\reved ->
              return (capped, reved)
            ) =<< rev
          ) =<< cap

-- quickChecks
main :: IO ()
main = do
  quickCheck ((\s -> composed s == fmapped s) :: [Char] -> Bool)
  quickCheck ((\s -> tupled1 s == tupled2 s) :: [Char] -> Bool)
  quickCheck ((\s -> tupled1 s == tupled3 s) :: [Char] -> Bool)
  quickCheck ((\s -> tupled1 s == tupled4 s) :: [Char] -> Bool)
  quickCheck ((\s -> tupled1 s == tupled5 s) :: [Char] -> Bool)
  quickCheck ((\s -> tupled1 s == tupled6 s) :: [Char] -> Bool)
