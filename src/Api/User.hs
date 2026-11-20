{-# LANGUAGE OverloadedStrings #-}

module Api.User where

import Api.Helpers
import Control.Concurrent (MVar)
import Database.SQLite.Simple (Connection)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Domain.User as User
import Network.HTTP.Types.Status (status201, status400, status404)
import qualified Storage.Users as Store
import Web.Scotty

userRoutes :: MVar Connection -> ScottyM ()
userRoutes db = do
  get "/user/:id" $ do
    uid <- readParam "id"
    runDb db (`Store.getUser` uid) >>= maybe (rejectEntry status404 "User not found") json
  get "/user/:name/search" $ do
    name <- readName
    runDb db (`Store.searchUserByName` name) >>= json
  post "/user/:name" $ do
    name <- readName
    saved <- runDb db (`Store.createUser` name)
    status status201
    setHeader "Location" (TL.pack ("/user/" ++ show (User.id saved)))
    json saved

readName :: ActionM T.Text
readName = do
  name <- T.strip <$> pathParam "name"
  if T.null name
    then rejectEntry status400 "Name must not be blank"
    else pure name
