-- 11-18d-LanguageExercises.hs
--
-- 11.18 Chapter Exercises, page 452
-- Language Exercises, page 456
--
module LanguageExercises where

import Data.Char
import Data.List

-- 1. Write a function that capitalizes a word.

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c : cs) = toUpper c : cs


-- 2. Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
-- Reuse the capitalizeWord function.

splitSentences :: String -> [String]
splitSentences text =
    let indices = elemIndices '.' text
    in reverse $ splitSentences' indices text []
    where
        splitSentences' :: [Int] -> String -> [String] -> [String]
        splitSentences' _ "" acc = acc
        splitSentences' [] txt acc = txt : acc
        splitSentences' (index:indices) txt acc = splitSentences' newIndices rest (sentence : acc)
            where
                (sentence, rest) = splitAt (index) txt
                newIndices = map (\i -> i - index) indices
                                    
capitalizeParagraph :: String -> String
capitalizeParagraph = trimLast
                        . intercalate ". "
                        . map capitalizeWord
                        . map (dropWhile (not . isAlpha))
                        . splitSentences
                            where trimLast s = take (length s - 1) s


paragraph = "blah. woot ha.   "
main :: IO ()
main = do
    print "----- main ----------"
    print $ "paragraph: " ++ paragraph
    print $ "capitalized: " ++ capitalizeParagraph paragraph
    return ()
