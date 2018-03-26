-- 11-18e-PhoneExercise.hs
--
-- 11.18 Chapter Exercises, page 452
-- Phone Exercise, page 456
--
module PhoneExercise where

import Data.Char
import Data.List

-- Remember old-fashioned phone inputs for writing text where you had to press
-- a button multiple times to get different letters to come up?
-- You may still have to do this when you try to search for a movie to watch
-- using your television remote control. You’re going to write code
-- to translate sequences of button presses into strings and vice versa.

-- So! Here is the layout of the phone:
{-
        -------------------------------
        |  1      |  2ABC   |  3DEF   |
        -------------------------------
        |  4GHI   |  5JKL   |  6MNO   |
        -------------------------------
        |  7PQRS  |  8TUV   |  9WXYZ  |
        -------------------------------
        |  *^     |  0+_    |  #.,    |
        -------------------------------
-}


-- 1. Create a data structure that captures the phone layout above.
-- The data structure should be able to express enough of how the layout works
-- that you can use it to dictate the behavior of the functions in the following exercises.

type Digit = Char
type Presses = Int -- 1 and up

data Button = Button {
                    digit :: Digit,
                    letters :: String
                } deriving (Eq, Show)

data Phone = Phone {
                buttons :: [Button]
             } deriving (Eq, Show)

validDigits :: String
validDigits = ['0'..'9'] ++ "*#"

isValidDigit :: Char -> Bool
isValidDigit = (flip elem) validDigits

validLetters :: String
validLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ."

isValidLetter :: Char -> Bool
isValidLetter = (flip elem) validLetters

phone :: Phone
phone = Phone
  [ Button '1' "",     Button '2' "abc", Button '3' "def"
  , Button '4' "ghi",  Button '5' "jkl", Button '6' "mno"
  , Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz"
  , Button '*' "",     Button '0' " ",   Button '#' "."
  ]

buttonOfDigit :: Digit -> Button
buttonOfDigit digit = head $ filter (\(Button d _) -> d == digit) $ buttons phone
  
lettersOfDigit :: Digit -> String
lettersOfDigit digit = letters $ buttonOfDigit digit
 
buttonOfLetter :: Digit -> Button
buttonOfLetter letter = head $ filter (\(Button digit letters) -> elem letter letters) $ buttons phone
   

-- 2. Convert the following conversations into the keypresses required to express them.
-- We’re going to suggest types and functions to fill in order to accomplish the goal,
-- but they’re not obligatory. If you want to do it differently, go right ahead.

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps char
    | not $ isValidLetter char    = []                                          -- invalid char (is not on the keyboard)
    | isValidDigit char           = [(char, length (lettersOfDigit char) + 1)]  -- pressed one of "1234567890*#"
    | isUpper char                = case findPresses (toLower char) of          -- upper case letter
                                        [] -> []
                                        list -> ('*', 1) : list
    | otherwise                   = findPresses char                            -- lower case letter or ' '
    where
        findPresses :: Char -> [(Digit, Presses)]
        findPresses letter =
            map (\index -> (digit $ buttonOfLetter letter, index + 1))
                $ elemIndices letter
                $ letters
                $ buttonOfLetter letter

testReverseTaps =
    reverseTaps '1' == [('1',1)] &&
    reverseTaps 'a' == [('2',1)] &&
    reverseTaps 'b' == [('2',2)] &&
    reverseTaps 'c' == [('2',3)] &&
    reverseTaps '2' == [('2',4)] &&
    reverseTaps 'A' == [('*',1),('2',1)] &&
    reverseTaps 'B' == [('*',1),('2',2)] &&
    reverseTaps 'C' == [('*',1),('2',3)] &&
    reverseTaps ' ' == [('0',1)] &&
    reverseTaps '0' == [('0',2)] &&
    reverseTaps '.' == [('#',1)] &&
    reverseTaps '*' == [] &&
    reverseTaps '#' == []
    
cellPhonesDead :: String -> [(Digit, Presses)]
cellPhonesDead = concatMap reverseTaps

testCellPhonesDead =
    cellPhonesDead "Abc x.*#" == [('*',1),('2',1),('2',2),('2',3),('0',1),('9',2),('#',1)]


-- 3. How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

testFingerTaps =
    (fingerTaps $ cellPhonesDead "Abc x.*#") == 11

keyPressesConvo = map (fingerTaps . cellPhonesDead) convo

testKeyPressesConvo =
    keyPressesConvo == [48,5,17,71,15,49,58,15,60]


-- 4. What was the most popular letter for each message? What was its cost?
-- You’ll want to combine reverseTaps and fingerTaps to figure out what it costs in taps.
-- reverseTaps is a list because you need to press a different button in order to get capitals.

mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy (\s1 s2 -> orderByLength s1 s2) . group . sort . filter isAlpha
    where
        orderByLength :: String -> String -> Ordering
        orderByLength s1 s2 = if length s1 < length s2 then LT else if length s1 == length s2 then EQ else GT

testMostPopularLetter = (mostPopularLetter $ head convo) == 'n'

mostPopularLettersConvo = map mostPopularLetter convo

mostPopularLettersConvoWithCost = zip mostPopularLettersConvo
                                    $ map (fingerTaps . cellPhonesDead)
                                    $ map (\x -> [x]) mostPopularLettersConvo

testMostPopularLettersConvoWithCost =
    mostPopularLettersConvoWithCost == [('n',2),('a',1),('h',2),('o',3),('y',3),('r',3),('t',1),('y',3),('u',2)]

    
-- 5. What was the most popular letter overall? What was the most popular word?

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestLtrConvo = coolestLtr convo
coolestLtrConvoWithCost = (coolestLtrConvo, sum $ map snd $ reverseTaps coolestLtrConvo)

coolestWord :: [String] -> String
coolestWord = head
                . maximumBy (\s1 s2 -> if length s1 < length s2 then LT else if length s1 == length s2 then EQ else GT)
                . group
                . sort
                . concatMap words

coolestWordConvo = coolestWord convo
testCoolestWordConvo = coolestWordConvo == "Lol"

coolestWordConvoWithCost = (coolestWordConvo, fingerTaps $ cellPhonesDead coolestWordConvo)
testCoolestWordConvoWithCost = coolestWordConvoWithCost == ("Lol",10)


main :: IO ()
main = do
    print "----- main ----------"
    return ()
