-- 05-06a-TypeInference.hs
--
-- 5.6 Type Inference, page 142
-- TypeInference, page 144
--
module TypeInference where

-- here we specify the type signature.
f :: Num a => a -> a -> a
f x y = x + y + 3

-- no type signature:
-- f' :: Num a => a -> a -> a
-- The compiler infers the same type.
f' x y = x + y + 3
