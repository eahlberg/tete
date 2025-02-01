module Tete.Main (main) where

import Database.SQLite.Simple (withConnection)
import Options.Applicative
import System.Directory
import System.Posix.Env qualified as System
import Tete.Prelude
import Tete.Timer (Description (..), Name (..))
import Tete.Timer qualified as Timer

data Command
  = Start Name (Maybe Description)
  | Stop
  | List
  deriving (Show)

startParser :: Parser Command
startParser =
  Start <$> nameParser <*> descriptionParser
  where
    nameParser = fmap (Name . pack) (strArgument idm)
    descriptionParser = (fmap . fmap) (Description . pack) (optional (strArgument idm))

commandParser :: Parser Command
commandParser =
  subparser
    ( startCommand
        <> stopCommand
        <> listCommand
    )
  where
    startCommand = command "start" (info startParser (progDesc "Start a timer given a name and an optional description"))
    stopCommand = command "stop" (info (pure Stop) (progDesc "Stop running timer"))
    listCommand = command "list" (info (pure List) (progDesc "List all timers"))

getDbLocation :: IO String
getDbLocation =
  do
    let envVar = "TETE_DB_DIR"
    userSuppliedDir <- System.getEnv envVar
    case userSuppliedDir of
      Nothing -> do
        appDir <- getXdgDirectory XdgData "tete"
        let shouldCreateParents = False
            dbLocation = appDir <> "/records.db"
        createDirectoryIfMissing shouldCreateParents appDir
        pure dbLocation
      Just userSuppliedLocation ->
        pure userSuppliedLocation

runCommand :: Command -> IO ()
runCommand cmd = do
  dbLocation <- getDbLocation
  withConnection dbLocation $ \conn -> do
    _ <- Timer.checkAndRunMigrations conn
    case cmd of
      List -> do
        Timer.list conn
      Stop -> do
        Timer.stop conn
      Start name mDescription -> do
        Timer.start conn name mDescription

main :: IO ()
main = runCommand =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Tete time tracker"
            <> header "Tete - a CLI for tracking time"
        )
