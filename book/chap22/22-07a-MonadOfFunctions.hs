-- 22-07a-MonadOfFunctions.hs
--
-- 22.7 The Monad of functions, page 878
--
{-# LANGUAGE InstanceSigs #-}

module MonadOfFunctions where

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+ 1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- foo and bar combined to one function froot
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+ 1) r, length r)

-- abstracting [] to Foldable
-- and removing one arg (compared to 'bar')
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- using foo in barPlus
barPlus :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

-- more compactly, also using foo and making (foo r) the first arg of bar
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- using an anonymous function to make it look more Reader-y
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- abstracting foo and bar away gives us fooBind
fooBind :: (t2 -> t1) -> (t1 -> t2 -> t) -> t2 -> t
fooBind m k = \r -> k (m r) r

-- making it a bit more Reader-y again by renaming the types t2, t1, t
fooBind' :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind' m k = \r -> k (m r) r

-- this is bind for functions
-- (>>=) :: Monad m => (m    a) -> (a -> (m    b)) -> (m    b)
(>>=)    ::            (r -> a) -> (a -> (r -> b)) -> (r -> b)
(>>=) m k = \r -> k (m r) r
