-- 12-05c-ValidateTheWord.hs
--
-- 12.5 Chapter Exercises, page 480
-- Validate the word, page 481
--
module ValidateTheWord where
    
import Data.Char
    
-- Use the Maybe type to write a function that counts the number of vowels
-- in a string and the number of consonants. If the number of vowels exceeds
-- the number of consonants, the function returns Nothing. In many human languages,
-- vowels rarely exceed the number of consonants so when they do, it may indicate
-- the input isn’t a word (that is, a valid input to your dataset):

newtype Word' = Word' String deriving (Eq, Show)

lowerVowels = "aeiou"
vowels = lowerVowels ++ map toUpper lowerVowels

isVowel :: Char -> Bool
isVowel c = elem c vowels

isConsonsant :: Char -> Bool
isConsonsant c = isAlpha c && not (isVowel c)

numberVowelsAndConsonsts :: String -> (Int, Int)
numberVowelsAndConsonsts str = count str 0 0
    where
        count :: String -> Int -> Int -> (Int, Int)
        count "" vowels consonants = (vowels, consonants)
        count (c:cs) vowels consonants
                | isVowel c = count cs (vowels+1) consonants
                | isAlpha c = count cs vowels (consonants+1) 
                | otherwise = count cs vowels consonants

hasMoreVowels :: String -> Bool
hasMoreVowels str =
    let (vowels, consonants) = numberVowelsAndConsonsts str
    in vowels > consonants

hasMoreVowels2 :: String -> Bool
hasMoreVowels2 str = head $ map
                        (\(vowels, consonants) -> vowels > consonants)
                        [numberVowelsAndConsonsts str]
    
mkWord :: String -> Maybe Word'
mkWord str
        | hasMoreVowels2 str = Nothing
        | otherwise = Just $ Word' str
