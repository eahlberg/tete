{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Data.Time
import Test.Hspec
import Tete.Timer.PrettyPrint qualified as PrettyPrint
import Tete.Timer.Stats qualified as Stats
import Tete.Timer.Timer
import Prelude

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 2019 10 1) 0

jan1 :: DiffTime -> UTCTime
jan1 = UTCTime (fromGregorian 2020 1 1)

main :: IO ()
main = hspec $ do
  describe "taskText" $ do
    it "should return data for a stopped timer" $ do
      let timer =
            Timer
              { timerId = 2,
                timerStartTime = jan1 0,
                timerStopTime = Just $ jan1 100,
                timerCreatedAt = dummyTime,
                timerUpdatedAt = Nothing,
                timerDescription = Just "stopped timer"
              }

      let task =
            Task
              { taskId = 1,
                taskName = "foo-bar",
                taskCreatedAt = jan1 0,
                taskUpdatedAt = Nothing,
                taskPeriods = Set.fromList [timer]
              }

      let now = jan1 200
      let actual = PrettyPrint.tasksText now [task]

      actual `shouldSatisfy` isInfixOf "foo-bar"
      actual `shouldSatisfy` isInfixOf "2020-01-01 00:00"
      actual `shouldSatisfy` isInfixOf "2020-01-01 00:01"
      actual `shouldSatisfy` isInfixOf "stopped timer"

    it "should return data for a running timer" $ do
      let timer =
            Timer
              { timerId = 2,
                timerStartTime = jan1 0,
                timerStopTime = Nothing,
                timerCreatedAt = dummyTime,
                timerUpdatedAt = Nothing,
                timerDescription = Just "running timer"
              }

      let task =
            Task
              { taskId = 1,
                taskName = "foo-bar",
                taskCreatedAt = jan1 0,
                taskUpdatedAt = Nothing,
                taskPeriods = Set.fromList [timer]
              }

      let now = dummyTime
      let actual = PrettyPrint.tasksText now [task]

      actual `shouldSatisfy` isInfixOf "foo-bar"
      actual `shouldSatisfy` isInfixOf "2020-01-01 00:00"
      actual `shouldSatisfy` isInfixOf "RUNNING"
      actual `shouldSatisfy` isInfixOf "running timer"

  describe "totalTime" $ do
    it "should return sum of start and stop if the timer has stopped" $ do
      let timer =
            Timer
              { timerId = 2,
                timerStartTime = jan1 0,
                timerStopTime = Just $ jan1 100,
                timerCreatedAt = dummyTime,
                timerUpdatedAt = Nothing,
                timerDescription = Nothing
              }
      let now = jan1 200
      let actual = Stats.totalTime now timer

      actual `shouldBe` secondsToNominalDiffTime 100

    it "should return sum of start and current time if the timer is running" $ do
      let timer =
            Timer
              { timerId = 2,
                timerStartTime = jan1 0,
                timerStopTime = Nothing,
                timerCreatedAt = dummyTime,
                timerUpdatedAt = Nothing,
                timerDescription = Nothing
              }
      let now = jan1 50
      let actual = Stats.totalTime now timer

      actual `shouldBe` secondsToNominalDiffTime 50
