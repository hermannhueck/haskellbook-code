-- 18-06b-KleisliIO.hs
--
-- 18.06 Application and Composition, page 783
-- IO example for Kleisli composition, page 787
--
--
module KleisliIO where

import Control.Monad ((>=>))

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM    -- using Kleisi operator

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "
