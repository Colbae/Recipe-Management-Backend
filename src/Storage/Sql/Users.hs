{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Users (userExists) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

userExists :: Query
userExists = fromString $(makeRelativeToProject "sql/users/exists.sql" >>= embedStringFile)
