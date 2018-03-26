-- 16-04-Functors1.hs
--
-- 16.4 Let’s talk about f, baby, page 631
-- A shining star for you to see, page 635
--
module Functor1 where

data FixMePls
  = FixMe
  | Pls
  deriving (Eq, Show)

-- Does not compile!
instance Functor FixMePls where
  fmap = error "it doesn't matter, it won't compile"
