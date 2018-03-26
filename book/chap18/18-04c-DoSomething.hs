-- 18-04c-DoSomething.hs
--
-- 18.04 Examples of Monad use, page 759
-- Do sonething, page 764
--
--
module DoSomething where

import Test.Hspec

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("10191-" ++ show i)

doSomething :: Integer -> (Maybe Integer, Maybe Integer, Maybe String)
doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

doSomething' :: Integer -> Maybe (Integer, Integer, String)
doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

main :: IO ()
main =
  hspec $ do
    context "Comparing the behaviour of doSomething and doSomething' ..." $ do
      describe "... passing the EVEN Integer '4':" $ do
        it "doSomething 4 `shouldBe` (Just 4,Just 5,Just \"10191-4\")" $ do
          doSomething 4 `shouldBe` (Just 4, Just 5, Just "10191-4")
        it "doSomething' 4 `shouldBe` Just (4, 5, \"10191-5\")" $ do
          doSomething' 4 `shouldBe` Just (4, 5, "10191-5")
      describe "... passing the ODD Integer '5':" $ do
        it "doSomething 5 `shouldBe` (Just 5, Nothing, Just \"10191-5\")" $ do
          doSomething 5 `shouldBe` (Just 5, Nothing, Just "10191-5")
        it "doSomething' 5 `shouldBe` Nothing" $ do
          doSomething' 5 `shouldBe` Nothing
