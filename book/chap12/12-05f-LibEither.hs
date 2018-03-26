-- 12-05f-LibEither.hs
--
-- 12.5 Chapter Exercises, page 480
-- Small library for Either, page 485
--
module LibEither where

-- Write each of the following functions. If more than one possible unique function exists
-- for the type, use common sense to determine what it should do.
    
-- 1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr.

-- recursive implementation
lefts' :: [Either a b] -> [a]
lefts' []               = []
lefts' ((Left x):rest)  = x : lefts' rest
lefts' (_:rest)         = lefts' rest

-- implementation with foldr
lefts'' :: [Either a b] -> [a]
lefts''  = foldr f []
        where
                f :: Either a b -> [a] -> [a]
                f (Left a) acc  = a : acc
                f _ acc         = acc


-- 2. Same as the last one. Use foldr eventually.

rights' :: [Either a b] -> [b]
rights' []       = []
rights' ((Right x):rest)        = x : rights' rest
rights' (_:rest)                = rights' rest

-- implementation with foldr
rights'' :: [Either a b] -> [b]
rights''  = foldr f []
        where
                f :: Either a b -> [b] -> [b]
                f (Right b) acc = b : acc
                f _ acc         = acc


-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
        where
                f :: Either a b -> ([a], [b]) -> ([a], [b])
                f (Left a) (as, bs)     = (a:as, bs)
                f (Right b) (as, bs)    = (as, b:bs)



-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)         = Nothing
eitherMaybe' f (Right b)        = Just $ f b


-- 5. This is a general catamorphism for Either values.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)    = f a
either' _ g (Right b)   = g b


-- 6. Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g = either' (const Nothing) (Just . g)
