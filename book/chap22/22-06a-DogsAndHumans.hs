-- 22-06a-DogsAndHumans.hs
--
-- 22.6 Functions have an Applicative too, page 873
-- Demonstrating the function Applicative, page 874
--
module DogsAndHumans where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Test.QuickCheck

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

instance Arbitrary Person where
  arbitrary = do
    hn <- HumanName <$> arbitrary
    dn <- DogName <$> arbitrary
    ad <- Address <$> arbitrary
    return $ Person hn dn ad

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader ('person' appears as parameter of the function 'getDog'.)
-- This version cannot be implemented in pointless style!!!
getDog :: Person -> Dog
getDog person = Dog (dogName person) (address person)

-- with Reader (Using pointless style the Person argument is not 'visible'.)
-- Using an applicative context we can use pointless style!!!
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- redefining fmap (<$>) for functions
(<$->>) :: (a -> b) ->(r -> a) -> (r -> b)
(<$->>) = (<$>)

-- redefining ap (<*>) for functions
(<*->>)::(r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

-- with Reader using the new operators
getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address

-- with Reader using 'liftA2'
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

--
-- The following comes from chapter 22.7, page 882
--

-- with Reader Monad using do syntax
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

-- with Reader Monad using bind (>>=)
getDogRM' :: Person -> Dog
getDogRM' = dogName >>= \name ->
            address >>= \addy ->
            return $ Dog name addy

-- with Reader Monad using liftM2
getDogRM'' :: Person -> Dog
getDogRM'' = liftM2 Dog dogName address

-- with Reader Monad using flipped bind (=<<)
getDogRM''' :: Person -> Dog
getDogRM''' = (\name ->
                (\addy ->
                  return $ Dog name addy
                ) =<< address
              ) =<< dogName 

main :: IO ()
main = do
  quickCheck ((\p -> getDog p == getDogR p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogR' p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogR'' p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogRM p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogRM' p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogRM'' p) :: Person -> Bool)
  quickCheck ((\p -> getDog p == getDogRM''' p) :: Person -> Bool)
