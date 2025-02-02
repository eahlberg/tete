module Tete.Timer.Stats (timerTotalTime, taskTotalTime) where

import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Time
import Tete.Prelude
import Tete.Timer.Timer

timerTotalTime :: UTCTime -> Timer -> NominalDiffTime
timerTotalTime now Timer {..} =
  let stopTime = fromMaybe now timerStopTime
   in diffUTCTime stopTime timerStartTime

timersTotalTime :: UTCTime -> [Timer] -> NominalDiffTime
timersTotalTime now timers = sum $ timerTotalTime now <$> timers

taskTotalTime :: UTCTime -> Task -> NominalDiffTime
taskTotalTime now task = timersTotalTime now timers
  where
    timers = Set.toList $ taskPeriods task
