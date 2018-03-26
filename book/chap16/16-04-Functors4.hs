-- 16-04-Functors4.hs

-- 16.4 Let’s talk about f, baby, page 631
-- A shining star for you to see what your 𝑓 can truly be, page 639

module Functor4 where

data FixMePls a = FixMe | Pls a
                  deriving (Eq, Show)

-- Does not compile!
instance Functor (FixMePls a) where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)
        