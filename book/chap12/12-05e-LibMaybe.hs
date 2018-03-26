-- 12-05e-LibMaybe.hs
--
-- 12.5 Chapter Exercises, page 480
-- Small library for Maybe, page 483
--
module LibMaybe where
        
-- 1. Simple boolean checks for Maybe values.

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust _        = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True

isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- 2. The following is the Maybe catamorphism. You can turn a Maybe
-- value into anything else with this.

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a


-- 3. In case you just want to provide a fallback value.

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1

-- Without the Maybe catamorphism
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe defaultt Nothing = defaultt

-- Using the Maybe catamorphism
fromMaybe2 :: a -> Maybe a -> a
fromMaybe2 defaultt mb = mayybee defaultt id mb


-- 4. Converting between List and Maybe.
-- As natural as any competitive bodybuilder

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


-- 5. For when we want to drop the Nothing values from our list.

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []

fromJust :: Maybe a -> a
fromJust Nothing = error "It's not a Just, but a Nothing"
fromJust (Just x) = x

catMaybes :: [Maybe a] -> [a]
catMaybes mbs = map fromJust $ filter isJust mbs

-- recursive solution
catMaybes2 :: [Maybe a] -> [a]
catMaybes2 []           = []
catMaybes2 (Nothing:xs) = catMaybes2 xs
catMaybes2 (Just x:xs)  = x : catMaybes2 xs


-- 6. You’ll see this called “sequence” later.

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe lmb
        | any isNothing lmb = Nothing
        | otherwise = Just $ map fromJust lmb

-- solution with foldr
flipMaybe2 :: [Maybe a] -> Maybe [a]
flipMaybe2 xs = foldr f (Just []) xs
    where
        f _ Nothing             = Nothing
        f Nothing _             = Nothing
        f (Just x) (Just xs)    = Just (x : xs)
