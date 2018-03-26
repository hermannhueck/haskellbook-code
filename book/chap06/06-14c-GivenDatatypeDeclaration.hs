-- 06-14c-GivenDatatypeDeclaration.hs
--
-- 6.14 Chapter Exercises, page 206
-- Given a datatype declaration, what can we do? page 208
--
module GivenDatatypeDeclaration where

-- Given the following datatype declarations
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- Which of the following will typecheck?
-- For the ones that don't typecheck, why don't they?

-- 1.
-- phew = Papu "chases" True
-- Data constructor Papu requires args of type Rocks and Yeah, not [Char] and Bool.
-- fixed:
phew = Papu (Rocks "chases") (Yeah True)

-- 2.
truth = Papu (Rocks "chomskydoz") (Yeah True) -- ok

-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p' -- ok, Papu has an instance of Eq

-- 4.
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- error: Papu has no instance of Ord
