module Tete.Timer.Format
  ( formatElapsedTime,
    formatTimestamp,
  )
where

import Data.Time (NominalDiffTime, defaultTimeLocale, nominalDiffTimeToSeconds)
import qualified Data.Time as Time
import Tete.Prelude

formatTimestamp :: UTCTime -> String
formatTimestamp = Time.formatTime defaultTimeLocale "%F %R"

formatElapsedTime :: NominalDiffTime -> String
formatElapsedTime diff = diffFormatted
  where
    diffFormatted = Time.formatTime defaultTimeLocale formatString diff
    diffSeconds = nominalDiffTimeToSeconds diff
    formatString = mkFormatString diffSeconds
    mkFormatString seconds
      | seconds < 60 = "%Ss"
      | seconds >= 60 && seconds < 3600 = "%Mmin"
      | seconds >= 3600 && seconds < 86400 = "%Hh %Mmin"
      | otherwise = "%Dd %Hh %Mmin"
