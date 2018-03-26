-- 17-07b-QuickBatchApplicative.hs
--
-- 17.07 You knew this was coming, page 727
-- QuickBatch Applicative, page 729
--
--
module QuickBatchApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "\nTesting Applicative with Checkers ..."
  putStrLn "\n1. Providing a sample instance, just to determine the type of the values to be generated"
  putStrLn "quickBatch (applicative [(\"b\", \"w\", (1::Int))])"
  quickBatch (applicative [("b", "w", (1::Int))])
  putStrLn "\n2. Or providing a type buttom which quickBatch uses only to determine the type of the generated values"
  putStrLn "quickBatch (applicative (undefined  :: [(String, String, Int)]))"
  quickBatch (applicative (undefined  :: [(String, String, Int)]))
  