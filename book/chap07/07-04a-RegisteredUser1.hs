-- 07-04a-RegisteredUser1.hs
--
-- 7.4 Pattern matching, page 226
-- RegisteredUser1, page 229
--
module RegisteredUser1 where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber
