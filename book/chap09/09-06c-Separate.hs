-- 09-06c-Separate.hs
--
-- 9.6 Extracting portions of lists, page 308
-- Exercises: The Fearful Symmetry 3, page 311
--
module Separate where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

-- Implement this
separate :: Char -> String -> [String]
separate _ "" = []
separate ch str =
  takeWhile (/= ch) str : separate ch (trim . dropWhile (/= ch) $ str)
  where
    trim s = case s of
      ch:rest -> rest
      x -> s

myLines :: String -> [String]
myLines str = separate '\n' str

myWords :: String -> [String]
myWords str = separate ' ' str


separate2 :: Char -> String -> [String]
separate2 _ "" = []
separate2 ch xs = case dropWhile (== '\n') xs of
                      [] -> []
                      ys -> line : separate2 ch rest
                            where (line, rest) = break (== '\n') ys

myLines2 :: String -> [String]
myLines2 str = separate2 '\n' str

myWords2 :: String -> [String]
myWords2 str = separate2 ' ' str


-- What we want 'myLines sentences' -- to equal
shouldEqual =
       [ "Tyger Tyger, burning bright"
       , "In the forests of the night"
       , "What immortal hand or eye"
       , "Could frame thy fearful symmetry?"
       ]

-- The main function here is a small test
-- to ensure you've written your function -- correctly.
main :: IO ()
main = do
  putStrLn $ "Are they equal (with myLines)?   " ++ show (myLines sentences == shouldEqual)
  putStrLn $ "Are they equal (with myLines2)?  " ++ show (myLines2 sentences == shouldEqual)
  return ()
