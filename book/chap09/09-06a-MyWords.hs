-- 09-06a-MyWords.hs
--
-- 9.6 Extracting portions of lists, page 308
-- Exercises: The Fearful Symmetry 1, page 311
--
module MyWords where

isBlank :: Char -> Bool
isBlank c = c == ' '

notBlank :: Char -> Bool
notBlank c = c /= ' '

myWords :: [Char] -> [[Char]]
myWords str = reverse (snd(myWords' str []))
  where
    myWords' :: [Char] -> [[Char]] -> ([Char], [[Char]])
    myWords' "" acc = ("", acc)
    myWords' str acc = myWords' (rest str) (firstWord str : acc)
      where
        firstWord = takeWhile notBlank . trimFront
        rest = dropWhile notBlank . trimFront
        trimFront = dropWhile isBlank


-- improved solution
myWords2 :: String -> [String]
myWords2 [] = []
myWords2 (' ':xs) = myWords2 xs
myWords2 xs =  takeWhile (/= ' ') xs : myWords2 (dropWhile (/= ' ') xs)
