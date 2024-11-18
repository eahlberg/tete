{-# LANGUAGE DeriveGeneric #-}

module Tete.Timer.DB (getRunningTimer, stopTimer, shouldRunMigrations, runMigrations, startTimer, TaskRow (..), TimerRow (..), getTaskRows, insertTimer, getTimerByName) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.Types (Null)
import Tete.Prelude

data TaskRow = TaskRow
  { taskRowId :: Int,
    taskRowName :: Text,
    taskRowCreatedAt :: UTCTime,
    taskRowUpdatedAt :: Maybe UTCTime
  }
  deriving (Generic, Show)

data TimerRow = TimerRow
  { timerRowId :: Int,
    timerRowTimerId :: Int,
    timerRowStartTime :: UTCTime,
    timerRowStopTime :: Maybe UTCTime,
    timerRowCreatedAt :: UTCTime,
    timerRowUpdatedAt :: Maybe UTCTime,
    timerRowDescription :: Maybe Text
  }
  deriving (Generic, Show)

instance FromRow TaskRow

instance FromRow TimerRow

instance FromRow (Maybe TimerRow) where
  fromRow = (nullVal *> nullVal *> nullVal *> nullVal *> nullVal *> nullVal *> nullVal) $> Nothing <|> (Just <$> fromRow)
    where
      nullVal = field :: RowParser Null

toTuple :: (a :. b) -> (a, b)
toTuple (x :. y) = (x, y)

getTaskRows :: Connection -> IO [(TaskRow, Maybe TimerRow)]
getTaskRows conn = do
  res <- query_ conn selectTimerAndTimerRows
  pure $ toTuple <$> res
  where
    selectTimerAndTimerRows :: Query
    selectTimerAndTimerRows = "SELECT * FROM task LEFT OUTER JOIN timer ON task.id = timer.task_id"

getTimerByName :: Connection -> Text -> IO (Maybe TaskRow)
getTimerByName conn name =
  query conn selectTimerByName (Only name)
    >>= \case
      [] -> pure Nothing
      [taskRow] -> pure $ Just taskRow
      taskRows -> error $ "DB: expected one task row with the name, found multiple: " <> show taskRows
  where
    selectTimerByName = "SELECT * FROM task WHERE name = ?"

insertTimer :: Connection -> Text -> IO ()
insertTimer conn name =
  execute conn "INSERT INTO task (name) VALUES (?)" (Only name)

startTimer :: Connection -> Int -> Maybe Text -> IO ()
startTimer conn taskRowId description =
  execute conn "INSERT INTO timer (task_id, start_time, description) VALUES (?, CURRENT_TIMESTAMP, ?)" (taskRowId, description)

getRunningTimer :: Connection -> IO (Maybe TimerRow)
getRunningTimer conn =
  query_ conn selectStartedTimers
    >>= \case
      [] -> pure Nothing
      [timer] -> pure (Just timer)
      _ -> error "DB: found multiple running timers, expected zero or one"
  where
    selectStartedTimers :: Query
    selectStartedTimers = "SELECT * FROM timer WHERE start_time NOT NULL AND stop_time IS NULL"

stopTimer :: Connection -> Int -> IO ()
stopTimer conn timerRowId = do
  executeNamed conn updateTimer [":id" := timerRowId]
  where
    updateTimer = "UPDATE timer SET stop_time = CURRENT_TIMESTAMP WHERE id = :id"

-- Migrations
createTaskTable :: Query
createTaskTable =
  "CREATE TABLE IF NOT EXISTS task (id integer PRIMARY KEY UNIQUE, name text NOT NULL, created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at timestamp);"

createTimerTable :: Query
createTimerTable = "CREATE TABLE IF NOT EXISTS timer (id integer PRIMARY KEY UNIQUE, task_id integer integer NOT NULL, start_time timestamp NOT NULL, stop_time timestamp, created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, updated_at timestamp, description text, FOREIGN KEY (task_id) REFERENCES task (id));"

createTimerNameUniqueIndex :: Query
createTimerNameUniqueIndex = "CREATE UNIQUE INDEX task_name_unique_index ON task (name);"

allMigrations :: [(Int, [Query])]
allMigrations =
  [ (1, [createTaskTable, createTimerTable, createTimerNameUniqueIndex])
  ]

createDbVersionTableQuery :: Query
createDbVersionTableQuery = "CREATE TABLE IF NOT EXISTS db_version (id integer PRIMARY KEY UNIQUE, version integer NOT NULL)"

updateDbVersionQuery :: Query
updateDbVersionQuery = "INSERT INTO db_version (version) VALUES (?)"

runMigrations :: Connection -> IO ()
runMigrations conn =
  withTransaction conn $ do
    forM_ allMigrations runMigration
  where
    runMigration (version, migrations) = do
      forM_ migrations (execute_ conn)
      updateDbVersion conn version

updateDbVersion :: Connection -> Int -> IO ()
updateDbVersion conn version =
  execute conn updateDbVersionQuery (Only version)

createDbVersionTable :: Connection -> IO ()
createDbVersionTable conn =
  execute_ conn createDbVersionTableQuery

shouldRunMigrations :: Connection -> IO Bool
shouldRunMigrations conn = do
  _ <- createDbVersionTable conn
  currentVersion <- getDbVersion conn
  pure $ currentVersion < pure latestVersion
  where
    latestVersion = maximum $ fst <$> allMigrations

newtype DbVersionRow = DbVersionRow {unDbVersionRow :: Maybe Int} deriving (Generic, Show)

instance FromRow DbVersionRow

getDbVersion :: Connection -> IO (Maybe Int)
getDbVersion conn =
  query_ conn selectMaxDbVersion
    >>= \case
      [versionRow] -> pure $ unDbVersionRow versionRow
      _ -> error "Failed to query max db version"
  where
    selectMaxDbVersion = "SELECT MAX(version) FROM db_version"
