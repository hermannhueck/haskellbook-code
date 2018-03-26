-- 11-13a-ConstructDestruct.hs
--
-- 11.13 Constructing and deconstructing values, page 421
-- Examples, page 421 ff
--
module ConstructDestruct where

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b
                } deriving (Eq, Show)

-- Sum and Product

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig
-- Farmhouse and Farmhouse' are the same.


-- nested Product

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


-- nested Sum

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal =
     Cow CowInfo
     | Pig PigInfo
     | Sheep SheepInfo
     deriving (Eq, Show)
 -- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


-- Getting it right
bess' = (CowInfo "Bess" 4)
bess = First bess' :: Animal'

e' = Second (SheepInfo "Elmer" 5 5)
elmer = Second e' :: Animal'

-- Making a mistake
elmo' = Second (SheepInfo "Elmo" 5 5)
-- elmo = First elmo' :: Animal'


-- Constructing values

trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- note:
-- MkId :: a -> Id a
idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
-- type Name = String      -- already defined above

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

type SN = Sum Twitter AskFm
-- Second Twitter :: SN      -- error, Second expects an arg of type AskFm
-- First AskFm :: SN         -- error, First expects an arg of type Twitter


-- writing SocialNetwork as a data type of its own
-- this is the preferable solution
data SocialNetwork = Twitter' | AskFm' deriving (Eq, Show)


-- using type synonyms for Twitter and AskFm
-- type synonyms are not type safe !!!!!
-- errors are not detected by the compiler !!!!!

type Twitter'' = String
type AskFm'' = String

twitter :: Sum Twitter'' AskFm''
twitter = First "Twitter"

askfm :: Sum Twitter'' AskFm''
askfm = First "AskFm" -- error cannot be detected by the compiler


-- record syntax

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' =
    RecordProduct { pfirst = 42
                  , psecond = 0.00001
                  }


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
