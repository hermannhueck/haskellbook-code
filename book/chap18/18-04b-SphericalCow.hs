-- 18-04b-SphericalCow.hs
--
-- 18.04 Examples of Monad use, page 759
-- Using the Maybe Monad, page 762
--
--
module SphericalCow where

import Test.Hspec

data Cow = Cow
  { name :: String
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

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

-- using do syntax
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- using (>>=)
-- Stack up the nested lambdas.
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>= \nammy ->
    noNegative age' >>= \agey ->
      noNegative weight' >>= \weighty -> weightCheck (Cow nammy agey weighty)

main :: IO ()
main =
  hspec $ do
    context "Testing the spherical cow ..." $ do
      describe "... with mkSphericalCow:" $ do
        it "mkSphericalCow \"Bess\" 5 499 `shouldBe` Just (Cow \"Bess\" 5 499)" $ do
          mkSphericalCow "Bess" 5 499 `shouldBe` Just (Cow "Bess" 5 499)
        it "mkSphericalCow \"Bess\" 5 500 `shouldBe` Nothing" $ do
          mkSphericalCow "Bess" 5 500 `shouldBe` Nothing
        it "mkSphericalCow \"Bless\" 5 500 `shouldBe` Just (Cow \"Bless\" 5 500)" $ do
          mkSphericalCow "Bless" 5 500 `shouldBe` Just (Cow "Bless" 5 500)
      describe "... with mkSphericalCow':" $ do
        it "mkSphericalCow' \"Bess\" 5 499 `shouldBe` Just (Cow \"Bess\" 5 499)" $ do
          mkSphericalCow' "Bess" 5 499 `shouldBe` Just (Cow "Bess" 5 499)
        it "mkSphericalCow' \"Bess\" 5 500 `shouldBe` Nothing" $ do
          mkSphericalCow' "Bess" 5 500 `shouldBe` Nothing
        it "mkSphericalCow' \"Bless\" 5 500 `shouldBe` Just (Cow \"Bless\" 5 500)" $ do
          mkSphericalCow' "Bless" 5 500 `shouldBe` Just (Cow "Bless" 5 500)
      describe "... with mkSphericalCow:''" $ do
        it "mkSphericalCow'' \"Bess\" 5 499 `shouldBe` Just (Cow \"Bess\" 5 499)" $ do
          mkSphericalCow'' "Bess" 5 499 `shouldBe` Just (Cow "Bess" 5 499)
        it "mkSphericalCow'' \"Bess\" 5 500 `shouldBe` Nothing" $ do
          mkSphericalCow'' "Bess" 5 500 `shouldBe` Nothing
        it "mkSphericalCow'' \"Bless\" 5 500 `shouldBe` Just (Cow \"Bless\" 5 500)" $ do
          mkSphericalCow'' "Bless" 5 500 `shouldBe` Just (Cow "Bless" 5 500)
