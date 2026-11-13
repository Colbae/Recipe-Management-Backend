module Main (main) where

import Api (routes)
import Api.Profiles (disabledProfiles, headerProfiles)
import Control.Concurrent.MVar (newMVar)
import qualified Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple (withConnection)
import Integration.Usda (newClient)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
  profileMode <- Data.Maybe.fromMaybe "header" <$> lookupEnv "PROFILE_MODE"
  profiles <- case profileMode of
    "disabled" -> pure disabledProfiles
    "header" -> pure headerProfiles
    _ -> ioError (userError "PROFILE_MODE must be disabled or header")
  apiKey <- fmap (T.strip . T.pack) <$> lookupEnv "USDA_API_KEY"
  base <- Data.Maybe.fromMaybe "https://api.nal.usda.gov/fdc/v1" <$> lookupEnv "USDA_BASE_URL"
  manager <- newManager tlsManagerSettings
  let usda = case apiKey of
        Just key | not (T.null key) -> Just (newClient manager key (reverse (dropWhile (== '/') (reverse base))))
        _ -> Nothing
  createDirectoryIfMissing True (takeDirectory database)
  withConnection database $ \connection -> do
    initialize connection
    db <- newMVar connection
    putStrLn ("Recipe API listening on port " ++ show port ++ "; database: " ++ database)
    scotty port (routes db usda profiles)
