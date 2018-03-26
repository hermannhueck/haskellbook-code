-- 19-03a-TimeOffset.hs
--
-- 19.03 Functor, page 798
-- Lifting over IO, page 798
--
--
module TimeOffset where

import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime
