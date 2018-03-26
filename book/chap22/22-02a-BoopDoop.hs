-- 22-02a-BoopDoop.hs
--
-- 22.2 A new beginning, page 861
--
module BoopDoop where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Test.QuickCheck

-- boop :: Integer -> Integer
boop = (*2)
-- doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

-- functor of functions
assert x = fmap boop doop x == (*2) ((+10) x)
-- when this x comes along, it's the first necessary argument to (+10)
-- then the result for that is the first necessary argument to (*2)

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

boopIDoop :: Integer -> Integer
boopIDoop = boop >>= \a ->
            doop >>= \b ->
            return (a + b)

-- using flip bind (looks a bit weird)
boopIDooWop :: Integer -> Integer
boopIDooWop = (\a ->
                (\b ->
                  return (a + b)
                ) =<< doop
              ) =<< boop

boopIDooWa :: Integer -> Integer
boopIDooWa = liftM2 (+) boop doop

main :: IO ()
main = do
  quickCheck ((\x -> bip x == (*2) ((+10) x)) :: Integer -> Bool)
  quickCheck ((\x -> bloop x == (*2) ((+10) x)) :: Integer -> Bool)
  quickCheck ((\x -> bloop x == bip x) :: Integer -> Bool)
  quickCheck ((\x -> bbop x == (x*2) + (x+10)) :: Integer -> Bool)
  quickCheck ((\x -> duwop x == (x*2) + (x+10)) :: Integer -> Bool)
  quickCheck ((\x -> bbop x == duwop x) :: Integer -> Bool)
  quickCheck ((\x -> duwop x == boopDoop x) :: Integer -> Bool)
  quickCheck ((\x -> boopDoop x == boopIDoop x) :: Integer -> Bool)
  quickCheck ((\x -> boopDoop x == boopIDooWop x) :: Integer -> Bool)
  quickCheck ((\x -> boopDoop x == boopIDooWa x) :: Integer -> Bool)
