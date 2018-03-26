module HangmanLib where

import Data.List (intersperse)
import System.Random (randomRIO)

-- Generating a word list and choosing a random word

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList $ lines dict


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where
      gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList words) = do
    randomIndex <- randomRIO (0, length words - 1)
    return $ words !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord -- same as: do wl <- gameWords; randomWord wl

-- Making a puzzle

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

instance Show Puzzle
  where
    show (Puzzle word discovered guessed) =
      (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed ++ " ----- " ++ word
 
freshPuzzle :: String -> Puzzle
freshPuzzle wordToGuess = Puzzle
                              wordToGuess
                              (map (const Nothing) wordToGuess)
                              []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordToGuess _ _) char = elem char wordToGuess

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
    where
        zipper :: Char -> Char -> Maybe Char -> Maybe Char
        zipper guessed wordChar guessChar =
            if wordChar == guessed then Just wordChar else guessChar
        newFilledInSoFar :: [Maybe Char]
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
  do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn "You already guessed that character, pick something else!"
          return puzzle
      (True, _) -> do
          putStrLn "This character was in the word, filling in the word accordingly"
          return (fillInCharacter puzzle guess)
      (False, _) -> do
          putStrLn "This character wasn't in the word, try again."
          return (fillInCharacter puzzle guess)
