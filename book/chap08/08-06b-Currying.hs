-- 08-06b-Currying.hs
--
-- 8.6 Chapter Exercises, page 294
-- Reviewing currying, page 295
--
module Currying where

-- Given the following definitions, tell us waht value results from further application.

cattyConny :: String -> String -> String
cattyConny x y = x ++ "-mrow-" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

test = appedCatty "woohoo!" == "woops-mrow-woohoo!"
  && frappe "1" == "1-mrow-haha"
  && appedCatty (frappe "blue") == "woops-mrow-blue-mrow-haha"
  && cattyConny (frappe "pink")
                (cattyConny "green" (appedCatty "blue"))
                == "pink-mrow-haha-mrow-green-mrow-woops-mrow-blue"
  && cattyConny (flippy "Pugs" "are") "awesome" == "are-mrow-Pugs-mrow-awesome"