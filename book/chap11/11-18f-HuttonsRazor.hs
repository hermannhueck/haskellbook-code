-- 11-18f-HuttonsRazor.hs
--
-- 11.18 Chapter Exercises, page 452
-- Hutton's Razor, page 459
--
module HuttonsRazor where

import Data.Char
import Data.List

-- Hutton’s Razor is a very simple expression language that expresses integer literals
-- and addition of values in that expression language. The “trick” to it is that it’s
-- recursive and the two expressions you’re summing together could be literals or themselves
-- further addition operations. This sort of datatype is stereotypical of expression lan-guages
-- used to motivate ideas in research papers and functional pearls. Evaluating or folding
-- a datatype is also in some sense what you’re doing most of the time while programming anyway.
-- See: http://www.cs.nott.ac.uk/~pszgmh/bib.html#semantics


-- 1. Your first task is to write the “eval” function which reduces an expression to a final sum.

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit value) = value
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)

-- Example of expected output:

-- Prelude> eval (Add (Lit 1) (Lit 9001))
-- 9002

testEval = eval (Add (Lit 1) (Lit 9001)) == 9002


-- 2. Write a printer for the expressions.

printExpr :: Expr -> String
printExpr (Lit value) = show value
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)

-- Expected output:

-- Prelude> printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"
-- Prelude> let a1 = Add (Lit 9001) (Lit 1)
-- Prelude> let a2 = Add a1 (Lit 20001)
-- Prelude> let a3 = Add (Lit 1) a2
-- Prelude> printExpr a3
-- "1 + 9001 + 1 + 20001"

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
a3String = printExpr a3

testPrintExpr =
    printExpr (Add (Lit 1) (Lit 9001)) == "1 + 9001"
    && a3String == "1 + 9001 + 1 + 20001"


main :: IO ()
main = do
    print "----- main ----------"
    return ()
