module TestCiphers
    ( testCeasar
    , testVignere
    ) where

import Ciphers
import Test.QuickCheck

-- test caesar cipher

testCeasar = quickCheck unceasarCaesaredTextIsText

unceasarCaesaredTextIsText :: Int -> String -> Bool
unceasarCaesaredTextIsText shift text = (uncaesar shift (caesar shift text)) == text


-- test vignère cipher

testVignere = quickCheck unvignereVigneredTextIsText

unvignereVigneredTextIsText :: Int -> String -> Bool
unvignereVigneredTextIsText keyword text = (uncaesar keyword (caesar keyword text)) == text