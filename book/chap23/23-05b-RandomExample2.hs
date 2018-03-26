-- 23-05b-RandomExample2.hs
--
-- 23.5 Throw down, page 898
-- Random Example improved, page 900
--
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- Six-sided die
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- type of state function
-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- type of repeat
-- repeat :: a -> [a]

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- type of replicateM
-- replicateM :: Applicative m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

---------------------------------------------------------------------------------
-- Exercises: Roll Your Own

-- 1. Refactor 'rollsToGetTwenty' into having the limit be a function argument.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise = go (sum + die) (count + 1) nextGen
        where (die, nextGen) = randomR (1, 6) gen

-- 2. Change 'rollsToGetN' to recording the swries of die that occurred
--    in addition to the count.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 [] g
  where
    go :: Int -> [Int] -> StdGen -> (Int, [Die])
    go sum dies gen
      | sum >= limit = (length dies, fmap intToDie $ reverse dies)
      | otherwise = go (sum + die) (die:dies) nextGen
        where (die, nextGen) = randomR (1, 6) gen
