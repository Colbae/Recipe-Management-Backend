{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Users (userExists, createUser, getUser, searchUserByName) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

userExists :: Query
userExists = fromString $(makeRelativeToProject "sql/users/exists.sql" >>= embedStringFile)

createUser :: Query
createUser = fromString $(makeRelativeToProject "sql/users/create_user.sql" >>= embedStringFile)

getUser :: Query
getUser = fromString $(makeRelativeToProject "sql/users/get_user.sql" >>= embedStringFile)

searchUserByName :: Query
searchUserByName = fromString $(makeRelativeToProject "sql/users/search_user_by_name.sql" >>= embedStringFile)