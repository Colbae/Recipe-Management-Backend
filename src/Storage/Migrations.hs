{-# LANGUAGE OverloadedStrings #-}

module Storage.Migrations (Migration (..), migrate) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import Database.SQLite.Simple
import qualified Database.SQLite3 as SQLite
import qualified Storage.Sql as Sql

-- Scripts are trusted, embedded application code. SQLite executes the entire
-- script, including multiple statements and triggers; do not split on ';'.
data Migration = Migration
  { migrationVersion :: Int,
    migrationName :: Text,
    migrationScript :: Text
  }
  deriving (Eq, Show)

migrate :: Connection -> [Migration] -> IO ()
migrate connection migrations = do
  unless (map migrationVersion migrations == [1 .. length migrations]) $
    ioError (userError "Migrations must be numbered consecutively starting at 1")
  -- Lock before reading history, so concurrent startups cannot apply a migration
  -- twice. The whole pending batch and its history commit or roll back together.
  withImmediateTransaction connection $ do
    execute_ connection Sql.createMigrationTable
    applied <- query_ connection Sql.listMigrations :: IO [(Int, Text)]
    let expected = map (\m -> (migrationVersion m, migrationName m)) migrations
    unless (applied == take (length applied) expected && length applied <= length expected) $
      ioError (userError "Database migration history does not match this executable")
    forM_ (drop (length applied) migrations) $ \migration -> do
      SQLite.exec (connectionHandle connection) (migrationScript migration)
      execute
        connection
        Sql.recordMigration
        (migrationVersion migration, migrationName migration)
