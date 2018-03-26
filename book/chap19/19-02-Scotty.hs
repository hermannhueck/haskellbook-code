-- 19-02-Scotty.hs
--
-- 19.02 Monoid, page 793
-- Templating content in Scotty, page 793
--
--
{-# LANGUAGE OverloadedStrings #-}

module Scotty where
    
import Data.Monoid (mconcat)
import Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- When the web server is running ... go to
-- http://localhost:3000/beam
--
-- see also https://github.com/scotty-web/scotty
