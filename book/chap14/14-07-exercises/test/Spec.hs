module Main where

import WordNumberTest (testWordNumber)
import QuickCheckArithmeticProperties (checkArithmeticProperties)
import FoolGen (foolGenEquallyDistributed, foolGenWithDistributionRatio2To1)
import Test.QuickCheck.Gen
import TestCiphers (testCeasar, testVignere)

main :: IO ()
main = do
    putStrLn "\n----- 14.7 Chapter Exercises, page 573 -----"
    putStrLn "\n----- Validating numbers into words, page 573, Testing WordNumber with Hspec -----"
    testWordNumber
    putStrLn "\n----- Using QuickCheck, page 574, Testing arithmetic properties with QuickCheck -----"
    checkArithmeticProperties
    putStrLn "\n----- Random Generator for Type 'Fool', page 577 -----"
    putStrLn "\n----- ----- Generating equally distributed Fool values:"
    sample foolGenEquallyDistributed
    putStrLn "\n----- ----- Generating Fool values with approximately twice as many Fulse than Frue:"
    sample foolGenWithDistributionRatio2To1
    putStrLn "\n----- QuickCheck Ceasar cipher, page 579 -----"
    testCeasar
    putStrLn "\n----- QuickCheck Vignère cipher, page 579 -----"
    testVignere
    putStrLn ""
    