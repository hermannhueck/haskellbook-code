-- 16-17-Ex2-RearrangeTypeConstructorArgs.hs

-- 16.17 Chapter Exercises, page 679
-- Exercises, page 680

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.


module PossibleValidFunctors where


-- 1.
data Sum' a b = First' a | Second' b      -- given
data Sum  b a = Second b | First   a      -- rearranged

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b


-- 2.
data Company' a b c = DeepBlue' a c | Something' b      -- given
data Company  a c b = DeepBlue  a c | Something  b      -- rearranged

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c


-- 3.
data More' a b = L' a b a | R' b a b deriving (Eq, Show)    -- given 
data More  b a = L  a b a | R  b a b deriving (Eq, Show)    -- rearranged

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'
