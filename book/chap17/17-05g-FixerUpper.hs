-- 17-05f-FixerUpper.hs
--
-- 17.05 Applicative in use, page 696
-- Exercise: FixerUpper, page 721
--
-- Given the function and values provided, use (<$>) from Functor
-- and (<*>) and pure from Applicative typeclass to fill in
-- missing bits of the broken code to make it work.
--
module FixerUpper where

-- 1.
-- wrong: const <$> Just "Hello" <*> "World"
ex1 :: Maybe String
ex1 = const <$> Just "Hello" <*> pure "World"
-- output: Just "Hello"

-- 2.
-- wrong: (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
ex2 :: Maybe (Integer, Integer, [Char], [Integer])
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
-- output: Just (90,10,"Tierness",[1,2,3])
