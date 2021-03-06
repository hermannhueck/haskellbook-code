-- 09-06b-PoemLines.hs
--
-- 9.6 Extracting portions of lists, page 308
-- Exercises: The Fearful Symmetry 2, page 311
--
module PoemLines where

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
myLines :: String -> [String]
myLines "" = []
myLines str =
  takeWhile (/= '\n') str : myLines (trim . dropWhile (/= '\n') $ str)
  where
    trim s = case s of
      '\n':rest -> rest
      x -> s

myLines2 :: String -> [String]
myLines2 "" = []
myLines2 xs = case dropWhile (== '\n') xs of
                      [] -> []
                      ys -> line : myLines2 rest
                            where (line, rest) = break (== '\n') ys

-- What we want 'myLines sentences' to equal
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
