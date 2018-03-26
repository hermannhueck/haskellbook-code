-- 17-09a-Ex1-SpecializeTypes.hs
--
-- 17.09 Chapter Exercises, page 741
-- Specialize Types, page 741
--
-- Given a type has an instance of Applicative, spcialize the types of the methods.
-- Test your specialization in the REPL. One way to do this is to bind aliases of the
-- typeclass methods to more concrete types that have the type we told you to fill in.
--
module SpecializeTypes where

-- 1. -- Type []
-- Methods
listPure :: a -> [a]
listPure = pure

listApply :: [(a -> b)] -> [a] -> [b]
listApply = (<*>)

-- 2. -- Type IO
-- Methods
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3. -- Type (,) a
-- Methods
pairPure :: Monoid a => b -> (,) a b
pairPure = pure
pairPure' :: Monoid a => b -> (a, b)
pairPure' = pure

pairApply :: Monoid a => (,) a (b -> c) -> (,) a b -> (,) a c
pairApply = (<*>)
pairApply' :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
pairApply' = (<*>)

-- 4. -- Type (->) e
-- Methods
funcPure :: a -> (->) e a
funcPure = pure
funcPure' :: a -> (e -> a)
funcPure' = pure

funcApply :: (->) e (a -> b) -> (->) e a -> (->) e b
funcApply = (<*>)
funcApply' :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
funcApply' = (<*>)
