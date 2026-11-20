module Storage.Users (userExists, createUser, getUser, searchUserByName) where

import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple
import Domain.User (User (User))
import qualified Storage.Sql.Users as Sql

fromStoredUser :: (Int64, Text) -> IO User
fromStoredUser (uid, name) =
  pure (User uid name)

userExists :: Connection -> Int64 -> IO Bool
userExists connection uid = do
  rows <- query connection Sql.userExists (Only uid) :: IO [Only Bool]
  pure (rows == [Only True])

createUser :: Connection -> Text -> IO User
createUser connection name = withTransaction connection $ do
  execute connection Sql.createUser (Only name)
  uid <- lastInsertRowId connection
  pure (User uid name)

getUser :: Connection -> Int64 -> IO (Maybe User)
getUser connection uid = do
  rows <- query connection Sql.getUser (Only uid)
  case rows of
    [] -> pure Nothing
    row : _ -> Just <$> fromStoredUser row

searchUserByName :: Connection -> Text -> IO [User]
searchUserByName connection name = do
  rows <- query connection Sql.searchUserByName (Only name)
  mapM fromStoredUser rows
