module Tete.Timer.Timer (Task (..), Timer (..)) where

import Tete.Prelude

data Timer = Timer
  { timerId :: Int,
    timerStartTime :: UTCTime,
    timerStopTime :: Maybe UTCTime,
    timerCreatedAt :: UTCTime,
    timerUpdatedAt :: Maybe UTCTime,
    timerDescription :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data Task = Task
  { taskId :: Int,
    taskName :: Text,
    taskCreatedAt :: UTCTime,
    taskUpdatedAt :: Maybe UTCTime,
    taskPeriods :: Set Timer
  }
  deriving (Show)
