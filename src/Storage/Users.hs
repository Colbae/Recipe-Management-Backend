module Storage.Users (userExists) where

import Data.Int (Int64)
import Database.SQLite.Simple
import qualified Storage.Sql.Users as Sql

userExists :: Connection -> Int64 -> IO Bool
userExists connection uid = do
  rows <- query connection Sql.userExists (Only uid) :: IO [Only Bool]
  pure (rows == [Only True])
