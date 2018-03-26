module Main where


import Test.Hspec
import HangmanLib


main :: IO ()
main = do
    putStrLn "\n----- 14.7 Chapter Exercises, Hangman testing, page 578 -----"
    testFillInCharacter
    testHandleGuess


testFillInCharacter :: IO ()
testFillInCharacter = hspec $ do
    describe "Testing fillInCharacter ..." $ do
        it "Guessing the puzzle should follow the hangman rules. Word to guess = wumpel" $ do
            let puzzle0 = Puzzle "wumpel" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] ""
            putStrLn $ ">>> puzzle0 = " ++ show puzzle0
            let puzzle1 = fillInCharacter puzzle0 'x'
            putStrLn $ ">>> puzzle1 = " ++ show puzzle1
            puzzle1 == Puzzle "wumpel" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] "x" `shouldBe` True
            let puzzle2 = fillInCharacter puzzle1 'x'
            putStrLn $ ">>> puzzle2 = " ++ show puzzle2
            puzzle2 == Puzzle "wumpel" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] "xx" `shouldBe` True
            let puzzle3 = fillInCharacter puzzle2 'y'
            putStrLn $ ">>> puzzle3 = " ++ show puzzle3
            puzzle3 == Puzzle "wumpel" [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] "yxx" `shouldBe` True
            let puzzle4 = fillInCharacter puzzle3 'u'
            putStrLn $ ">>> puzzle4 = " ++ show puzzle4
            puzzle4 == Puzzle "wumpel" [Nothing, Just 'u', Nothing, Nothing, Nothing, Nothing] "uyxx" `shouldBe` True
            let puzzle5 = fillInCharacter puzzle4 'l'
            putStrLn $ ">>> puzzle5 = " ++ show puzzle5
            puzzle5 == Puzzle "wumpel" [Nothing, Just 'u', Nothing, Nothing, Nothing, Just 'l'] "luyxx" `shouldBe` True


testHandleGuess :: IO ()
testHandleGuess = hspec $ do
    describe "Testing handleGuess ..." $ do
        it "---- DUMMY for handleGuess -----" $ do
            1 + 1 `shouldBe` 2
            