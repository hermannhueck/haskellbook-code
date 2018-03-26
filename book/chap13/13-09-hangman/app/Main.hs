module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)

import HangmanLib

maxGuesses :: Int
maxGuesses = 7

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > maxGuesses
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else
      return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else
      return ()
      
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"
      
main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

