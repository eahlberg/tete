{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Data.Time
import Test.Hspec
import Tete.Timer.PrettyPrint
import Tete.Timer.Timer
import Prelude

timer :: Timer
timer =
  Timer
    { timerId = 2,
      timerStartTime = UTCTime (fromGregorian 2020 1 1) 0,
      timerStopTime = Just (UTCTime (fromGregorian 2020 1 1) 100),
      timerCreatedAt = UTCTime (fromGregorian 2020 1 1) 0,
      timerUpdatedAt = Nothing,
      timerDescription = Just "foo bar timer period"
    }

task :: Task
task =
  Task
    { taskId = 1,
      taskName = "foo-bar",
      taskCreatedAt = UTCTime (fromGregorian 2020 1 1) 0,
      taskUpdatedAt = Nothing,
      taskPeriods = Set.fromList [timer]
    }

main :: IO ()
main = hspec $ do
  describe "taskText" $ do
    it "should pretty print a timer" $ do
      let actual = tasksText [task]

      True `shouldBe` False
      actual `shouldSatisfy` isInfixOf "foo-bar"
      actual `shouldSatisfy` isInfixOf "2020-01-01 00:00"
      actual `shouldSatisfy` isInfixOf "2020-01-01 00:01"
      actual `shouldSatisfy` isInfixOf "foo bar timer period"
