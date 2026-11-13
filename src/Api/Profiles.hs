{-# LANGUAGE OverloadedStrings #-}

module Api.Profiles (ProfileSelection (..), disabledProfiles, headerProfiles, requireCurrentProfile) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.User
import Network.HTTP.Types.Status (status400)
import qualified Storage.Users as Users
import Text.Read (readMaybe)
import Web.Scotty

-- Profiles organize shared data, selecting an ID does not authenticate anyone.
-- Intended for a trusted network where members may select any existing profile.
newtype ProfileSelection = ProfileSelection
  { resolveCurrentProfile :: ActionM (Maybe CurrentUser)
  }

disabledProfiles :: ProfileSelection
disabledProfiles = ProfileSelection (pure Nothing)

headerProfiles :: ProfileSelection
headerProfiles = ProfileSelection resolve
  where
    resolve = do
      claimed <- header "X-User-Id"
      pure (claimed >>= readMaybe . TL.unpack >>= mkCurrentUser)

requireCurrentProfile :: MVar Connection -> ProfileSelection -> ActionM CurrentUser
requireCurrentProfile db profiles = do
  candidate <- resolveCurrentProfile profiles
  case candidate of
    Nothing -> reject
    Just current -> do
      registered <- liftIO (withMVar db (\connection -> Users.userExists connection (currentUserId current)))
      if registered then pure current else reject
  where
    reject = do
      status status400
      json (object ["error" .= ("Missing, invalid, or unregistered profile ID; select an existing profile using X-User-Id" :: Text)])
      finish
