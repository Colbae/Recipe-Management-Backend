{-# LANGUAGE OverloadedStrings #-}

module Storage.Database (initialize) where

import Database.SQLite.Simple (Connection, execute_)
import Storage.Migrations (Migration (..), migrate)
import qualified Storage.Sql as Sql

initialize :: Connection -> IO ()
initialize connection = do
  execute_ connection Sql.busyTimeout
  migrate connection migrations

migrations :: [Migration]
migrations =
  [ Migration 1 "001_create_recipes.sql" Sql.initialRecipeSchema
  ]
