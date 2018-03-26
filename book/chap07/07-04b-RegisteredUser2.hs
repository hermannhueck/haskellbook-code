-- 07-04b-RegisteredUser2.hs
--
-- 7.4 Pattern matching, page 226
-- RegisteredUser2, page 230
--
module RegisteredUser2 where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
            putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
            putStrLn $ name ++ " " ++ show acctNum
