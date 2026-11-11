module Main (main) where

import Api (routes)
import Control.Concurrent.MVar (newMVar)
import qualified Data.Maybe
import Database.SQLite.Simple (withConnection)
import Storage.Database (initialize)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)
import Web.Scotty (scotty)

main :: IO ()
main = do
  database <- Data.Maybe.fromMaybe "database/recipes.sqlite3" <$> lookupEnv "DATABASE_PATH"
  portSetting <- lookupEnv "PORT"
  port <- case portSetting of
    Nothing -> pure 3000
    Just value -> case readMaybe value of
      Just p | p > 0 && p <= 65535 -> pure p
      _ -> ioError (userError "PORT must be an integer between 1 and 65535")
  createDirectoryIfMissing True (takeDirectory database)
  withConnection database $ \connection -> do
    initialize connection
    db <- newMVar connection
    putStrLn ("Recipe API listening on port " ++ show port ++ "; database: " ++ database)
    scotty port (routes db)
