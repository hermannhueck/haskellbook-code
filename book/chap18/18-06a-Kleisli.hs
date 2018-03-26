-- 18-06a-Kleisli.hs
--
-- 18.06 Application and Composition, page 783
-- Composition for monadic functions, page 784
--
--
module Kleisli where

import Control.Monad (join)
import Control.Monad ((>=>))

{- 
-- Composing functions the simple way will not compile!
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = f (g a)
-}

-- Composition with fmap and join works!
mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = join (f <$> (g a))

-- join and fmap together can be replaced by (>>=)
mcomp'' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f

-- this is just what the fish operator (kleisli op) does with args flipped.
mcomp''' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp''' = flip (>=>)

str = unlines ["This is", "a string", "spanning several lines"]

test = (lines str >>= words) == (mcomp' words lines str) &&
       (lines str >>= words) == (mcomp'' words lines str) &&
       (lines str >>= words) == (mcomp''' words lines str) &&
       (lines str >>= words) == ((lines >=> words) str)     -- (>=>) expects the args flipped.
