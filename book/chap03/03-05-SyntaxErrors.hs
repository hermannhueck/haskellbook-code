-- 03-05-SyntaxErrors.hs
--
-- 3.5 Types of concatenation functions, page 74
-- Exercises: Syntax Errors, page 76
--
module SyntaxErrors where
    
-- 1.   ++ [1,2,3] [4,5,6]
e1 = (++) [1,2,3] [4,5,6]

-- 2.   '<3' ++ ' Haskell'
e2 = "<3" ++ " Haskell"

-- 3.   concat ["<3", " Haskell"]   -- is correct
e3 = concat ["<3", " Haskell"]
