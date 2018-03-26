-- 18-02-Bind.hs
--
-- 18.02 Sorry -- a Monad is not a burrito, page 745
-- The answer is the exercise, page 750
--
-- Write bind in terms of fmap and join.
--
module Bind where

import Control.Monad (join)

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

bind' :: Monad m => (a -> m b) -> m a -> m b      -- the same with do syntax
bind' f ma = do
  a <- ma
  f a

-- test
andOne :: Integer -> [Integer]
andOne x = [x, 1]

list :: [Integer]
list = [4, 5, 6]

test =
  andOne 10 == [10, 1]
  && fmap andOne list == [[4, 1], [5, 1], [6, 1]]
  && bind andOne list == [4, 1, 5, 1, 6, 1]           -- using bind
  && bind' andOne list == [4, 1, 5, 1, 6, 1]          -- using bind'
  && (=<<) andOne list == [4, 1, 5, 1, 6, 1]          -- using equivalent operator (=<<) as prefix
  && (andOne =<< list) == [4, 1, 5, 1, 6, 1]          -- (=<<) as infix
  && (list >>= andOne) == [4, 1, 5, 1, 6, 1]          -- (>>=) is (=<<) flipped
  