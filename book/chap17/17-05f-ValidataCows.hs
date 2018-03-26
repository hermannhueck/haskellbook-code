-- 17-05e-ValidataCows.hs
--
-- 17.05 Applicative in use, page 696
-- Before we moooove on, page 716
--
--
module ValidataCows where

import Control.Applicative (liftA3)
import Test.Hspec

data Cow = Cow {
    name ::String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- Validating to get rid of empty strings, negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

-- cowFromString is bad. You can probably tell.
-- But by the use of Applicative, it can be improved!

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

-- using liftA3
cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')

-- tests with Hspec
main :: IO ()
main =
  hspec $ do
    describe "Testing validation of Cows with Maybe Applicative ..." $ do
      it "cowFromString \"Bess\" 5 300 `shouldBe` Just (Cow \"Bess\" 5 300)" $ do
        cowFromString "Bess" 5 300 `shouldBe` Just (Cow "Bess" 5 300)
      it "cowFromString' \"Bess\" 5 300 `shouldBe` Just (Cow \"Bess\" 5 300)" $ do
        cowFromString' "Bess" 5 300 `shouldBe` Just (Cow "Bess" 5 300)
      it "cowFromString'' \"Bess\" 5 300 `shouldBe` Just (Cow \"Bess\" 5 300)" $ do
        cowFromString'' "Bess" 5 300 `shouldBe` Just (Cow "Bess" 5 300)
      it "cowFromString \"Bess\" 5 (-300) `shouldBe` Nothing" $ do
        cowFromString "Bess" 5 (-300) `shouldBe` Nothing
      it "cowFromString' \"Bess\" (-1) 300 `shouldBe` Nothing" $ do
        cowFromString' "Bess" (-1) 300 `shouldBe` Nothing
      it "cowFromString'' \"\" 5 300 `shouldBe` Nothing" $ do
        cowFromString'' "" 5 300 `shouldBe` Nothing

-- examples from text using tie fighter:
cow1 = Cow <$> noEmpty "Bess"
cow2 = cow1 <*> noNegative 1
cow3 = cow2 <*> noNegative 2

cow1' = liftA3 Cow
cow2' = cow1' (noEmpty "muuuhhh")
cow3' = cow2' (noNegative 1)
cow4' = cow3' (noNegative 2)
