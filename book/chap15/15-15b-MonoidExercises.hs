-- 15-15b-MonoidExercises.hs
--
-- 15.15 Chapter Exercises, page 617
-- Monoid Exercises, page 620
--
module MonoidExercises where

import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.Hspec
import Test.QuickCheck hiding (Failure, Success) -- 11. defines it's own Failure and Success

---------------------------------------------------------------------------------------------
-- two short hands for the REPL
qc :: Testable prop => prop -> IO ()
qc = quickCheck

vc :: Testable prop => prop -> IO ()
vc = verboseCheck

-- Validate all Monoid instances
---------------------------------------------------------------------------------------------
-- the Monoid laws to validate
-- associativity law for Semigroups and Monoids
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- left identy law for Monoids
monoidLeftIdentiy :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentiy m = mempty <> m == m

-- right identy law for Monoids
monoidRightIdentiy :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentiy m = m <> mempty == m

---------------------------------------------------------------------------------------------
-- 1. Trivial
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivIdent = Trivial -> Bool

quickCheck_Trivial :: IO ()
quickCheck_Trivial = do
  putStrLn "1. Validating Trivial ..."
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentiy :: TrivIdent)
  quickCheck (monoidRightIdentiy :: TrivIdent)

---------------------------------------------------------------------------------------------
-- 2. Identity
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdentAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

type IdentIdent = Identity [Int] -> Bool

quickCheck_Identity :: IO ()
quickCheck_Identity = do
  putStrLn "2. Validating Identity ..."
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (monoidLeftIdentiy :: IdentIdent)
  quickCheck (monoidRightIdentiy :: IdentIdent)

---------------------------------------------------------------------------------------------
-- 3. Two
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) =>
         Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoAssoc = Two [Int] String -> Two [Int] String -> Two [Int] String -> Bool

type TwoIdent = Two [Int] String -> Bool

quickCheck_Two :: IO ()
quickCheck_Two = do
  putStrLn "3. Validating Two ..."
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentiy :: TwoIdent)
  quickCheck (monoidRightIdentiy :: TwoIdent)

---------------------------------------------------------------------------------------------
-- (Semi 4). Three
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b, Monoid c, Semigroup c) =>
         Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeAssoc
   = Three [Int] String (Sum Int) -> Three [Int] String (Sum Int) -> Three [Int] String (Sum Int) -> Bool

type ThreeIdent = Three [Int] String (Sum Int) -> Bool

quickCheck_Three :: IO ()
quickCheck_Three = do
  putStrLn "(Semi 4). Validating Three ..."
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentiy :: ThreeIdent)
  quickCheck (monoidRightIdentiy :: ThreeIdent)

---------------------------------------------------------------------------------------------
-- (Semi 5). Four
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  Four x y z zz <> Four x' y' z' zz' =
    Four (x <> x') (y <> y') (z <> z') (zz <> zz')

instance ( Monoid a
         , Semigroup a
         , Monoid b
         , Semigroup b
         , Monoid c
         , Semigroup c
         , Monoid d
         , Semigroup d
         ) =>
         Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    zz <- arbitrary
    return $ Four x y z zz

type FourAssoc
   = Four [Int] String (Sum Int) (Product Int) -> Four [Int] String (Sum Int) (Product Int) -> Four [Int] String (Sum Int) (Product Int) -> Bool

type FourIdent = Four [Int] String (Sum Int) (Product Int) -> Bool

quickCheck_Four :: IO ()
quickCheck_Four = do
  putStrLn "(Semi 5). Validating Four ..."
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentiy :: FourIdent)
  quickCheck (monoidRightIdentiy :: FourIdent)

---------------------------------------------------------------------------------------------
-- 4. BoolConj
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return $ BoolConj x

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjIdent = BoolConj -> Bool

quickCheck_BoolConj :: IO ()
quickCheck_BoolConj = do
  putStrLn "4. Validating BoolConj ..."
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentiy :: BoolConjIdent)
  quickCheck (monoidRightIdentiy :: BoolConjIdent)

---------------------------------------------------------------------------------------------
-- 5. BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return $ BoolDisj x

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjIdent = BoolDisj -> Bool

quickCheck_BoolDisj :: IO ()
quickCheck_BoolDisj = do
  putStrLn "5. Validating BoolDisj ..."
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentiy :: BoolDisjIdent)
  quickCheck (monoidRightIdentiy :: BoolDisjIdent)

---------------------------------------------------------------------------------------------
-- (Semi 8). Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

{-
    The Semigroup for Or should have the following behavior. We can think of this as having
    a “sticky” Snd value where it’ll hold onto the first Snd value when and if one is passed
    as an argument. This is similar to the First' Monoid you wrote earlier.

     Prelude> Fst 1 <> Snd 2
     Snd 2
     Prelude> Fst 1 <> Fst 2
     Fst 2
     Prelude> Snd 1 <> Fst 2
     Snd 1
     Prelude> Snd 1 <> Snd 2
     Snd 1
-}
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  Fst x <> Fst y = Fst y
  Fst x <> Snd y = Snd y
  Snd x <> _ = Snd x

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Or a b) where
  mempty = Fst mempty -- !!! There is no valid empty element. Hence 'Or' is not a valid Monoid!
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

type OrAssoc = Or [Int] String -> Or [Int] String -> Or [Int] String -> Bool

type OrIdent = Or [Int] String -> Bool

quickCheck_Or :: IO ()
quickCheck_Or = do
  putStrLn "(Semi 8). Validating Or ..."
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (monoidLeftIdentiy :: OrIdent)
  putStrLn "(Semi 8). Validating right identity of 'Or' should fail ..."
  quickCheck $ expectFailure (monoidRightIdentiy :: OrIdent)

---------------------------------------------------------------------------------------------
-- 6. Combine
{-
    Prelude> let f = Combine $ \n -> Sum (n + 1)
    Prelude> let g = Combine $ \n -> Sum (n - 1)
    Prelude> unCombine (f <> g) $ 0
    Sum {getSum = 0}
    Prelude> unCombine (f <> g) $ 1
    Sum {getSum = 2}
    Prelude> unCombine (f <> f) $ 1
    Sum {getSum = 4}
    Prelude> unCombine (g <> f) $ 1
    Sum {getSum = 2}

    Prelude> let f = Combine $ \n -> Sum (n + 1)
    Prelude> unCombine (mappend f mempty) $ 1
    Sum {getSum = 2}

    Hint: This function will eventually be applied to a single value of type a.
    But you’ll have multiple functions that can produce a value of type b.
    How do we combine multiple values so we have a single b? This one will probably be tricky!
    Remember that the type of the value inside of Combine is that of a function.
    The type of functions should already have an Arbitrary instance that you can reuse for testing this instance.
-}
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance Show (Combine a b) where
  show (Combine f) = "Combine { unCombine :: (a -> b) }"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- genCombine x = fmap ((flip ($) x ). unCombine) $ generate (arbitrary :: Gen (Combine Int Int))
type FuncAssoc f a = f -> f -> f -> a -> Bool

type FuncIdent f a = f -> a -> Bool

semigroupCombineAssoc :: (Eq b, Semigroup b) => FuncAssoc (Combine a b) a
semigroupCombineAssoc f g h x =
  unCombine ((f <> g) <> h) x == unCombine (f <> (g <> h)) x

monoidCombineLeftIdent ::
     (Eq b, Monoid b, Semigroup b) => FuncIdent (Combine a b) a
monoidCombineLeftIdent f x = unCombine (mempty <> f) x == unCombine f x

monoidCombineRightIdent ::
     (Eq b, Monoid b, Semigroup b) => FuncIdent (Combine a b) a
monoidCombineRightIdent f x = unCombine (f <> mempty) x == unCombine f x

quickCheck_Combine :: IO ()
quickCheck_Combine = do
  putStrLn "6. Validating Combine ..."
  quickCheck (semigroupCombineAssoc :: FuncAssoc (Combine Int String) Int)
  quickCheck (monoidCombineLeftIdent :: FuncIdent (Combine Int String) Int)
  quickCheck (monoidCombineRightIdent :: FuncIdent (Combine Int String) Int)

---------------------------------------------------------------------------------------------
-- 7. Comp
{-
    Hint: We can do something that seems a little more specific and natural to functions
    now that the input and output types are the same.
-}
newtype Comp a = Comp
  { unComp :: (a -> a)
  }

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ \x -> f x <> g x

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance Show (Comp a) where
  show (Comp f) = "Comp { unComp :: (a -> a) }"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

-- genComp x = fmap ((flip ($) x ). unComp) $ generate (arbitrary :: Gen (Comp Int))
semigroupCompAssoc :: (Eq a, Semigroup a) => FuncAssoc (Comp a) a
semigroupCompAssoc f g h x =
  unComp ((f <> g) <> h) x == unComp (f <> (g <> h)) x

monoidCompLeftIdent :: (Eq a, Monoid a, Semigroup a) => FuncIdent (Comp a) a
monoidCompLeftIdent f x = unComp (mempty <> f) x == unComp f x

monoidCompRightIdent :: (Eq a, Monoid a, Semigroup a) => FuncIdent (Comp a) a
monoidCompRightIdent f x = unComp (f <> mempty) x == unComp f x

quickCheck_Comp :: IO ()
quickCheck_Comp = do
  putStrLn "7. Validating Comp ..."
  quickCheck (semigroupCompAssoc :: FuncAssoc (Comp (Sum Int)) (Sum Int))
  quickCheck
    (semigroupCompAssoc :: FuncAssoc (Comp (Product Int)) (Product Int))
  quickCheck (semigroupCompAssoc :: FuncAssoc (Comp String) String)
  quickCheck (monoidCompLeftIdent :: FuncIdent (Comp (Sum Int)) (Sum Int))
  quickCheck
    (monoidCompLeftIdent :: FuncIdent (Comp (Product Int)) (Product Int))
  quickCheck (monoidCompLeftIdent :: FuncIdent (Comp String) String)
  quickCheck (monoidCompRightIdent :: FuncIdent (Comp (Sum Int)) (Sum Int))
  quickCheck
    (monoidCompRightIdent :: FuncIdent (Comp (Product Int)) (Product Int))
  quickCheck (monoidCompRightIdent :: FuncIdent (Comp String) String)

---------------------------------------------------------------------------------------------
-- (Semi 11). Validation
data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where -- type 'b' not required to be a Semigroup
  Success x <> _ = Success x
  _ <> Success x = Success x
  Failure x <> Failure y = Failure $ x <> y

instance (Monoid a, Semigroup a) => Monoid (Validation a b) where -- type 'b' not required to be a Monoid or Semigroup
  mempty = Failure mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    oneof $ map return [Failure x, Success y]

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

type ValidationAssoc
   = Validation String Int -> Validation String Int -> Validation String Int -> Bool

type ValidationIdent = Validation String Int -> Bool

quickCheck_Validation :: IO ()
quickCheck_Validation = do
  putStrLn "(Semi 11). Print Validation examples ..."
  print "success 1 <> failure \"blah\": "
  print (success 1 <> failure "blah")
  print "\"woot\" <> failure \"blah\": "
  print (failure "woot" <> failure "blah")
  print "success 1 <> success 2: "
  print (success 1 <> success 2)
  print "failure \"woot\" <> success 2: "
  print (failure "woot" <> success 2)
  putStrLn "(Semi 11). Validating Validation ..."
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (monoidLeftIdentiy :: ValidationIdent)
  quickCheck (monoidRightIdentiy :: ValidationIdent)

---------------------------------------------------------------------------------------------
-- 8. Mem
-- This next exercise will involve doing something that will feel a bit unnatural still and you may find it difficult.
-- If you get it and you haven’t done much FP or Haskell before, get yourself a nice beverage.
-- We’re going to toss you the instance declaration so you don’t churn on a missing Monoid constraint you didn’t know you needed.
newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) =
    Mem $ \s ->
      let (a1, s1) = f s
          (a2, s2) = g s1
      in (a1 `mappend` a2, s2)

instance Show (Mem s a) where
  show (Mem f) = "Mem { runMem :: s -> (a, s) }"

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

monoidMemAssoc :: (Monoid a, Eq a, Eq s) => FuncAssoc (Mem s a) s
monoidMemAssoc f g h x =
  runMem ((f `mappend` g) `mappend` h) x ==
  runMem (f `mappend` (g `mappend` h)) x

monoidMemLeftIdent :: (Monoid a, Eq a, Eq s) => FuncIdent (Mem s a) s
monoidMemLeftIdent f x = runMem (mempty `mappend` f) x == runMem f x

monoidMemRightIdent :: (Monoid a, Eq a, Eq s) => FuncIdent (Mem s a) s
monoidMemRightIdent f x = runMem (f `mappend` mempty) x == runMem f x

f' :: Num a => Mem a [Char]
f' = Mem $ \s -> ("hi", s + 1)

print_Mem :: IO ()
print_Mem = do
  putStrLn "8. Print Mem examples ..."
  let rmzero = runMem mempty 0
      rmleft = runMem (f' `mappend` mempty) 0
      rmright = runMem (mempty `mappend` f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

quickCheck_Mem :: IO ()
quickCheck_Mem = do
  print_Mem
  putStrLn "8. Validating Mem ..."
  quickCheck (monoidMemAssoc :: FuncAssoc (Mem Int String) Int)
  quickCheck (monoidMemLeftIdent :: FuncIdent (Mem Int String) Int)
  quickCheck (monoidMemRightIdent :: FuncIdent (Mem Int String) Int)

---------------------------------------------------------------------------------------------
-- main
main :: IO ()
main = do
  putStrLn "----- Validation of Monoid instances -----"
  quickCheck_Trivial
  quickCheck_Identity
  quickCheck_Two
  quickCheck_Three
  quickCheck_Four
  quickCheck_BoolConj
  quickCheck_BoolDisj
  quickCheck_Or
  quickCheck_Combine
  quickCheck_Comp
  quickCheck_Validation
  quickCheck_Mem
