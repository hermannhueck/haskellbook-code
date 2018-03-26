-- 07-06c-ArtfulDodgy.hs
--
-- 7.6 Higher order functions, page 239
-- Exercises: Artful Dodgy, page 246
--
module ArtfulDodgy where

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2
