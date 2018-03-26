-- 10-06-DatabaseProcessing.hs
--
-- 10.6 How to write fold functions, page 368
-- Exercises: DatabaseProcessing, page 371
--
module DatabaseProcessing where


-- Write the following functions for processing this data.
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 2
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]


-- 1. Write a function that filters for DbDate values and returns a list
--    of the UTCTime values inside them.

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getTimes []
        where
          getTimes :: DatabaseItem -> [UTCTime] -> [UTCTime]
          getTimes (DbDate time) dates = time : dates
          getTimes _ dates = dates


-- 2. Write a function that filters for DbNumber values and returns a list
--    of the Integer values inside them.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumbers []
        where
          getNumbers :: DatabaseItem -> [Integer] -> [Integer]
          getNumbers (DbNumber number) numbers = number : numbers
          getNumbers _ numbers = numbers


-- 3. Write a function that gets the most recent date.

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate


-- 4. Write a function that sums all of the DbNumber values.

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber


-- 5. Write a function that gets the average of the DbNumber values.

avgDb :: [DatabaseItem] -> Double
avgDb db = total / entries
  where total = (fromIntegral . sum) numbers
        entries = (fromIntegral . length) numbers
        numbers = filterDbNumber db


main :: IO ()
main = do
  print $ "filterDbDate --> " ++ show (filterDbDate theDatabase)
  print $ "filterDbNumber --> " ++ show (filterDbNumber theDatabase)
  print $ "mostRecent --> " ++ show (mostRecent theDatabase)
  print $ "sumDb --> " ++ show (sumDb theDatabase)
  print $ "avgDb --> " ++ show (avgDb theDatabase)
