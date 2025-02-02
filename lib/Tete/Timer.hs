module Tete.Timer
  ( checkAndRunMigrations,
    list,
    start,
    stop,
    Description (..),
    Name (..),
  )
where

import Control.Monad.Extra (whenM)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Database.SQLite.Simple (Connection)
import Tete.Prelude
import Tete.Timer.DB (TaskRow (..), TimerRow (..), getTaskRows)
import Tete.Timer.DB qualified as DB
import Tete.Timer.PrettyPrint qualified as PrettyPrint
import Tete.Timer.Timer (Task (..), Timer (..))

newtype Name = Name {unName :: Text} deriving (Show)

newtype Description = Description {unDescription :: Text} deriving (Show)

timerFromDB :: TimerRow -> Timer
timerFromDB TimerRow {..} =
  Timer
    { timerId = timerRowId,
      timerStartTime = timerRowStartTime,
      timerStopTime = timerRowStopTime,
      timerCreatedAt = timerRowCreatedAt,
      timerUpdatedAt = timerRowUpdatedAt,
      timerDescription = timerRowDescription
    }

timersFromRows :: NonEmpty (TaskRow, Maybe TimerRow) -> [Task]
timersFromRows rows =
  let grouped = NE.groupBy (\(x, _) (y, _) -> taskRowId x == taskRowId y) rows
   in fmap timerFromRows grouped
  where
    timerFromRows :: NonEmpty (TaskRow, Maybe TimerRow) -> Task
    timerFromRows groupedRows =
      let (TaskRow {..}, _) = NE.head groupedRows
       in Task
            { taskId = taskRowId,
              taskName = taskRowName,
              taskCreatedAt = taskRowCreatedAt,
              taskUpdatedAt = taskRowUpdatedAt,
              taskPeriods = mkTimers $ mapMaybe snd $ NE.toList groupedRows
            }

mkTimers :: [TimerRow] -> Set Timer
mkTimers = Set.fromList . fmap timerFromDB

{- HLINT ignore "Redundant fmap" -}
getTimers :: Connection -> IO [Task]
getTimers conn =
  fmap NE.nonEmpty (getTaskRows conn)
    >>= \case
      Nothing -> pure []
      Just rows -> pure $ timersFromRows rows

list :: Connection -> IO ()
list conn = do
  timers <- getTimers conn
  now <- getCurrentTime
  putStrLn $ PrettyPrint.tasksText now timers

checkAndRunMigrations :: Connection -> IO ()
checkAndRunMigrations conn =
  whenM (DB.shouldRunMigrations conn)
    $ DB.runMigrations conn
    >> tputStrLn "Migrations were run."

data TimersState
  = TimerRunning TimerRow
  | NoTimerRunning

getTimersState :: Connection -> IO TimersState
getTimersState conn =
  DB.getRunningTimer conn
    >>= \case
      Just timerRow -> pure $ TimerRunning timerRow
      Nothing -> pure NoTimerRunning

insertTimer :: Connection -> Name -> IO TaskRow
insertTimer conn (Name name) =
  DB.insertTimer conn name
    >> DB.getTimerByName conn name
    >>= \case
      Just updatedTimer -> pure updatedTimer
      Nothing -> error "Shouldn't happen since we just inserted a timer"

startTimer :: Connection -> Name -> Maybe Description -> IO ()
startTimer conn name'@(Name name) mDescription = do
  TaskRow {taskRowId} <-
    DB.getTimerByName conn name
      >>= \case
        Nothing -> insertTimer conn name'
        Just taskRow -> pure taskRow
  DB.startTimer conn taskRowId (fmap unDescription mDescription)
  tputStrLn $ "Timer with name '" <> name <> "' started."

start :: Connection -> Name -> Maybe Description -> IO ()
start conn name mDescription =
  getTimersState conn
    >>= \case
      (TimerRunning TimerRow {timerRowId}) ->
        tputStrLn $ "timer with id '" <> tshow timerRowId <> "' already running."
      NoTimerRunning -> do
        startTimer conn name mDescription

stopTimer :: Connection -> TimerRow -> IO ()
stopTimer conn (TimerRow {timerRowId}) =
  DB.stopTimer conn timerRowId
    >> tputStrLn "Timer stopped."

stop :: Connection -> IO ()
stop conn =
  getTimersState conn
    >>= \case
      TimerRunning timerRow -> stopTimer conn timerRow
      NoTimerRunning -> tputStrLn "No timer is running."
