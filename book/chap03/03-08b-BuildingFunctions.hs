-- 03-08b-BuildingFunctions.hs
--
-- 3.8 Chapter Exercises, page 81
-- BuildingFunctions, page 82
--
module BuildingFunctions where

-- 1. a)
test1a1 = "Curry is awesome" ++ "!" == "Curry is awesome!"
test1a2 = concat ["Curry is awesome", "!"] == "Curry is awesome!"
    
-- 1. b)
test1b1 = ["Curry is awesome!" !! 4] == "y"
test1b2 = take 1 (drop 4 "Curry is awesome!") == "y"

-- 1. c)
test1c = drop 9 "Curry is awesome!" == "awesome!"

-- 2. a)
appendExclamation :: String -> String
appendExclamation str = str ++ "!"

test2a = appendExclamation "Hay" == "Hay!"

-- 2. b)
letter4 :: String -> String
letter4 str = take 1 (drop 4 str)

test2b = letter4 "Honolulu" == "l"

-- 2. c)
after9th :: String -> String
after9th str = drop 9 str

test2c = after9th "Honolulu is far away." == "is far away."

-- 3.
thirdLetter :: String -> Char
thirdLetter str = head $ drop 2 str

test3 = thirdLetter "Curry is awesome" == 'r'

thirdLetter' :: String -> Char
thirdLetter' str = str !! 2

test3' = thirdLetter' "Curry is awesome" == 'r'

-- 4.
letterAtIndex :: Int -> Char
letterAtIndex index = "Curry is awesome" !! index

test4 = letterAtIndex 2 == 'r'

-- 5.
rvrs :: String -> String
rvrs str = concat [s3, " ", s2, " ", s1]
  where
    s1 = take 5 str
    s2 = take 2 $ drop 6 str
    s3 = drop 9 str

test5 = rvrs "Curry is awesome" == "awesome is Curry"
