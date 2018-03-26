-- 16-17-Ex1-PossibleValidFunctors.hs
--
-- 16.17 Chapter Exercises, page 679
-- Exercises, page 679
--
-- Determine if a valid Functor can be written for the datatype provided
--
module PossibleValidFunctors where

import GHC.Arr

-- 1.
-- data Bool = False | True
-- No Functor possible! Bool has kind: *
-- 2.
data BoolAndSomethingElse a
  = False' a
  | True' a

-- A Functor can be implemented! BoolAndSomethingElse has kind: * -> *
instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3.
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- A Functor can be implemented! BoolAndSomethingElse has kind: * -> *
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4.
newtype Mu f = InF
  { outF :: f (Mu f)
  }

-- No Functor possible! Mu has kind: (* -> *) -> *
-- 5.
data D =
  D (Array Word Word)
    Int
    Int
-- No Functor possible! D has kind: *
