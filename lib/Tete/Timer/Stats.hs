module Tete.Timer.Stats where

import Data.Maybe (fromMaybe)
import Data.Time
import Tete.Prelude
import Tete.Timer.Timer

totalTime :: UTCTime -> Timer -> NominalDiffTime
totalTime now Timer {..} =
  let stopTime = fromMaybe now timerStopTime
   in diffUTCTime stopTime timerStartTime

timersTotalTime :: UTCTime -> [Timer] -> NominalDiffTime
timersTotalTime now timers = sum $ totalTime now <$> timers
