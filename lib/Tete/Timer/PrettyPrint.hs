module Tete.Timer.PrettyPrint (tasksText) where

import Data.Set qualified as Set
import Data.Text as Text
import Data.Time
import Tete.Prelude
import Tete.Timer.Stats qualified as Stats
import Tete.Timer.Timer
import Text.PrettyPrint.Boxes hiding ((<>))

tasksText :: UTCTime -> [Task] -> Text
tasksText now tasks =
  pack $ render $ vsep 1 left [taskBoxes now tasks]

taskBoxes :: UTCTime -> [Task] -> Box
taskBoxes now = vsep 1 left . fmap (taskBox now)

taskBox :: UTCTime -> Task -> Box
taskBox now Task {..} =
  let timerList = Set.toList taskPeriods
      header = [text (unpack taskName)]
      body =
        case timerList of
          [] -> [text "No timers"]
          _ -> timerBox <$> timerList
      footer =
        let total = Stats.timersTotalTime now timerList
         in [text (unpack (Text.intercalate ": " ["Total time", formatElapsedTime total]))]
   in vsep 0 left (header <> body <> footer)

timerBox :: Timer -> Box
timerBox Timer {..} =
  let timeFields =
        case timerStopTime of
          Nothing ->
            ["RUNNING"]
          Just stopTime ->
            [formatTime' stopTime, formatElapsedTime (diffUTCTime stopTime timerStartTime)]
      fields =
        [ formatTime' timerStartTime
        ]
      desc =
        case timerDescription of
          Just description -> [description]
          Nothing -> []
      boxes = fmap (text . unpack) (fields <> timeFields <> desc)
   in punctuateH left (char '|') boxes

formatTime' :: UTCTime -> Text
formatTime' = pack . formatTime defaultTimeLocale "%F %R"

formatElapsedTime :: NominalDiffTime -> Text
formatElapsedTime diff = pack diffFormatted
  where
    diffFormatted = formatTime defaultTimeLocale formatString diff
    diffSeconds = nominalDiffTimeToSeconds diff
    formatString = mkFormatString diffSeconds
    mkFormatString seconds
      | seconds < 60 = "%Ss"
      | seconds >= 60 && seconds < 3600 = "%Mmin"
      | seconds >= 3600 && seconds < 86400 = "%Hh %Mmin"
      | otherwise = "%Dd %Hh %Mmin"
