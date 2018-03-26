-- 11-13b-Programmers.hs
--
-- 11.13 Constructing and deconstructing values, page 421
-- Exercise: Programmers, page 429
--
module Programmers where

data OperatingSystem =
       GnuPlusLinux
       | OpenBSDPlusNevermindJustBSDStill
       | Mac
       | Windows
       deriving (Eq, Show)

data ProgLang =
        Haskell
        | Agda
        | Idris
        | PureScript deriving (Eq, Show)

data Programmer =
     Programmer { os :: OperatingSystem
                , lang :: ProgLang
                } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

-- We can reorder stuff when we use record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

-- Write a function that generates all possible values of Programmer.
-- Use the provided lists of inhabitants of OperatingSystem and ProgLang.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
      [ GnuPlusLinux
      , OpenBSDPlusNevermindJustBSDStill , Mac
      , Windows
      ]

allLanguages :: [ProgLang]
allLanguages =
      [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

-- Programmer is a product of two types, you can determine how many
-- inhabitants of Programmer you have by calculating:

products = length allOperatingSystems * length allLanguages
test = products == length allProgrammers

-- This is the essence of how product types and the number of inhabitants relate.
