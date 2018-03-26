-- 17-05e-ValidataPersons.hs
--
-- 17.05 Applicative in use, page 696
-- Using the maybe Applicative, page 707
--
--
module ValidataPersons where

import Control.Applicative (liftA2)
import Test.Hspec

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen then Nothing else Just s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name
         Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' -> Just $ Person n' a'

-- improved solution with Maybe Applicative
mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

-- using liftA2
mkPerson'' :: String -> String -> Maybe Person
mkPerson'' n a = liftA2 Person (mkName n) (mkAddress a)

-- tests with Hspec
main :: IO ()
main =
  hspec $ do
    describe "Testing validation of Persons with Maybe Applicative ..." $ do
      it "mkPerson \"Babe\" \"old macdonald's\" `shouldBe` Just (Person (Name \"Babe\") (Address \"old macdonald's\"))" $ do
        mkPerson "Babe" "old macdonald's" `shouldBe` Just (Person (Name "Babe") (Address "old macdonald's"))
      it "mkPerson' \"Babe\" \"old macdonald's\" `shouldBe` Just (Person (Name \"Babe\") (Address \"old macdonald's\"))" $ do
        mkPerson' "Babe" "old macdonald's" `shouldBe` Just (Person (Name "Babe") (Address "old macdonald's"))
      it "mkPerson'' \"Babe\" \"old macdonald's\" `shouldBe` Just (Person (Name \"Babe\") (Address \"old macdonald's\"))" $ do
        mkPerson'' "Babe" "old macdonald's" `shouldBe` Just (Person (Name "Babe") (Address "old macdonald's"))
      it "mkPerson \"Babe with more than 25 characters\" \"old macdonald's\" `shouldBe` Nothing" $ do
        mkPerson "Babe with more than 25 characters" "old macdonald's" `shouldBe` Nothing
      it "mkPerson' \"Babe with more than 25 characters\" \"old macdonald's\" `shouldBe` Nothing" $ do
        mkPerson' "Babe with more than 25 characters" "old macdonald's" `shouldBe` Nothing
      it "mkPerson'' \"Babe with more than 25 characters\" \"old macdonald's\" `shouldBe` Nothing" $ do
        mkPerson'' "Babe with more than 25 characters" "old macdonald's" `shouldBe` Nothing
