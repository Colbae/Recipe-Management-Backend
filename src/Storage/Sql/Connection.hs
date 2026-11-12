{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Connection
  ( busyTimeout, foreignKeys ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

busyTimeout :: Query
busyTimeout = fromString $(makeRelativeToProject "sql/busy-timeout.sql" >>= embedStringFile)

foreignKeys :: Query
foreignKeys = fromString $(makeRelativeToProject "sql/foreign-keys.sql" >>= embedStringFile)
