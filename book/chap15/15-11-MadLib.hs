-- 15-11-MadLib.hs
--
-- 15.11 Madness, page 604
-- 
module MadLib where

import Data.Monoid
import Test.Hspec
import Test.QuickCheck

-- Write the Monoid instance for our Maybe type renamed to Optional.

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <> adv <> " as he jumped into his car " <> noun <>
  " and drove off with his " <>
  adj <>
  " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat $ zipWith (<>) [e, adv, noun, adj] template
  where
    template =
      [ "! he said "
      , " as he jumped into his car "
      , " and drove off with his "
      , " wife."
      ]

main :: IO ()
main = do
  putStrLn ""
  print $ "madlibbin':          " ++ madlibbin' "ouch" "stupidly" "cat" "brave"
  print $ "madlibbinBetter':    " ++ madlibbinBetter' "ouch" "stupidly" "cat" "brave"
  hspec $ do
    describe "Testing MadLib ..." $ do
      it "madlibbinBetter' gives the same result as madlibbin'" $ do
        madlibbinBetter' "ouch" "stupidly" "cat" "brave" `shouldBe` madlibbin' "ouch" "stupidly" "cat" "brave"
      it "madlibbinBetter' should work like madlibbin'" $ do
        property madlibbinBetterWorksLikeMadlibbin

madlibbinBetterWorksLikeMadlibbin :: Exclamation -> Adverb -> Noun -> Adjective -> Bool
madlibbinBetterWorksLikeMadlibbin e adv noun adj =
  madlibbinBetter' e adv noun adj == madlibbin' e adv noun adj
