-- 16-04-Functors2.hs

-- 16.4 Let’s talk about f, baby, page 631
-- A shining star for you to see what your 𝑓 can truly be, page 637

module Functor2 where

data FixMePls a = FixMe | Pls a
                  deriving (Eq, Show)

-- Compiles!
instance Functor FixMePls where
    fmap = error "it doesn't matter, it won't compile"
    