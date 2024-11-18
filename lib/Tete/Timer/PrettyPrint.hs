module Tete.Timer.PrettyPrint (tasksText) where

import Data.Set qualified as Set
import Data.Text as Text
import Data.Time
import Tete.Prelude
import Tete.Timer.Timer
import Text.PrettyPrint.Boxes hiding ((<>))

tasksText :: [Task] -> Text
tasksText tasks =
  pack $ render $ vsep 1 left [taskBoxes tasks]

taskBoxes :: [Task] -> Box
taskBoxes = vsep 1 left . fmap taskBox

taskBox :: Task -> Box
taskBox Task {..} =
  let timerList = Set.toList taskPeriods
      timerBoxes =
        case timerList of
          [] -> [text "No timers"]
          _ -> timerBox <$> timerList
      timerNameBox = text (unpack taskName)
   in vsep 0 left ([timerNameBox] <> timerBoxes)

timerBox :: Timer -> Box
timerBox Timer {..} =
  let timeFields =
        case timerStopTime of
          Nothing ->
            ["RUNNING"]
          Just stopTime ->
            [formatTime' stopTime, formatElapsedTime timerStartTime stopTime]
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

formatElapsedTime :: UTCTime -> UTCTime -> Text
formatElapsedTime startTime stopTime = pack diffFormatted
  where
    diffFormatted = formatTime defaultTimeLocale formatString diff
    diff = diffUTCTime stopTime startTime
    diffSeconds = nominalDiffTimeToSeconds diff
    formatString = mkFormatString diffSeconds
    mkFormatString seconds
      | seconds < 60 = "%Ss"
      | seconds >= 60 && seconds < 3600 = "%Mmin"
      | seconds >= 3600 && seconds < 86400 = "%Hh %Mmin"
      | otherwise = "%Dd %Hh %Mmin"
