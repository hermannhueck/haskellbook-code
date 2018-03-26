-- 13-14c-PalindromeForSentences.hs
--
-- 13.14 Chapter Exercises, page 531
-- Modifying code, Palindrome for sentences, page 332
--
module PalindromeForSentences where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlpha)

-- 3. If you tried using palindrome on a sentence such as “Madam I’m Adam,”
-- you may have noticed that palindrome checker doesn’t work on that.
-- Modifying the above so that it works on sentences, too, involves several steps.
-- You may need to refer back to previous examples in the chapter to get ideas
-- for proper ordering and nesting. You may wish to import Data.Char to use the function toLower. Have fun.

palindrome :: IO ()
palindrome = forever $ do
  line' <- getLine
  let line = map toLower $ filter isAlpha line  -- clean input
  case (line == reverse line) of
      True -> do
        putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess