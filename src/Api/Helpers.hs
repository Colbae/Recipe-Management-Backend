{-# LANGUAGE OverloadedStrings #-}

module Api.Helpers (runDb, readId, rejectEntry, readPantry) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Domain.Pantry (PantryInput (..))
import Domain.Recipe (Ingredient)
import Network.HTTP.Types
import Text.Read (readMaybe)
import Web.Scotty

runDb :: MVar Connection -> (Connection -> IO a) -> ActionM a
runDb db action = liftIO (withMVar db action)

readId :: ActionM Int64
readId = do
  value <- pathParam "id" :: ActionM T.Text
  case readMaybe (T.unpack value) of
    Just rid | rid > 0 -> pure rid
    _ -> do
      status status400
      json (object ["error" .= ("id must be a positive integer" :: T.Text)])
      finish

rejectEntry :: Status -> T.Text -> ActionM a
rejectEntry code message = do
  status code
  json (object ["error" .= message])
  finish

readPantry :: ActionM [Ingredient]
readPantry = do
  payload <- body
  case eitherDecode payload of
    Left errorMessage -> do
      status status400
      json (object ["errors" .= [T.pack errorMessage]])
      finish
    Right (PantryInput items) -> pure items
