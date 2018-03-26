-- 13-14b-Palindrome.hs
--
-- 13.14 Chapter Exercises, page 531
-- Modifying code, Palindrome, page 332
--
module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)

-- 2. Here is a very simple, short block of code. Notice it has a forever
-- that will make it keep running, over and over again. Load it into your REPL
-- and test it out. Then refer back to the chapter and modify it
-- to exit successfully a er a False result.

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  case (line == reverse line) of
      True -> do
        putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess