-- 16-04-Functors3.hs

-- 16.4 Let’s talk about f, baby, page 631
-- A shining star for you to see what your 𝑓 can truly be, page 638

module Functor3 where

data FixMePls a = FixMe | Pls a
                  deriving (Eq, Show)

-- Compiles!
instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)
        