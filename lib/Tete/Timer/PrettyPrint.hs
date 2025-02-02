module Tete.Timer.PrettyPrint (tasksText) where

import Data.List qualified as List
import Data.Set qualified as Set
import Data.Time
import Tete.Prelude
import Tete.Timer.Stats qualified as Stats
import Tete.Timer.Timer
import Text.PrettyPrint.Boxes hiding ((<>))

summaryText :: UTCTime -> [Task] -> String
summaryText now tasks = formatElapsedTime total
  where
    total = sum $ Stats.taskTotalTime now <$> tasks

tasksText :: UTCTime -> [Task] -> String
tasksText now tasks =
  render $ vsep 1 left [taskBoxes now tasks, text (summaryText now tasks)]

taskBoxes :: UTCTime -> [Task] -> Box
taskBoxes now = vsep 1 left . fmap (taskBox now)

taskBox :: UTCTime -> Task -> Box
taskBox now task@Task {..} =
  let timerList = Set.toList taskPeriods
      header = [text (unpack taskName)]
      body =
        case timerList of
          [] -> [text "No timers"]
          _ -> timerBox <$> timerList
      footer =
        let total = Stats.taskTotalTime now task
         in [text (List.intercalate ": " ["Total time", formatElapsedTime total])]
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
          Just description -> [unpack description]
          Nothing -> []
      boxes = fmap text (fields <> timeFields <> desc)
   in punctuateH left (char '|') boxes

formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "%F %R"

formatElapsedTime :: NominalDiffTime -> String
formatElapsedTime diff = diffFormatted
  where
    diffFormatted = formatTime defaultTimeLocale formatString diff
    diffSeconds = nominalDiffTimeToSeconds diff
    formatString = mkFormatString diffSeconds
    mkFormatString seconds
      | seconds < 60 = "%Ss"
      | seconds >= 60 && seconds < 3600 = "%Mmin"
      | seconds >= 3600 && seconds < 86400 = "%Hh %Mmin"
      | otherwise = "%Dd %Hh %Mmin"
