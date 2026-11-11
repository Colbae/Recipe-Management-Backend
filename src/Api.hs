{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api (routes) where

import Api.Recipes (recipeRoutes)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Network.HTTP.Types.Status (status404, status500)
import System.IO (hPutStrLn, stderr)
import Web.Scotty

routes :: MVar Connection -> ScottyM ()
routes db = do
  defaultHandler $ Handler $ \(err :: SomeException) -> do
    liftIO (hPutStrLn stderr ("Recipe API error: " ++ show err))
    status status500
    json (object ["error" .= ("Internal server error" :: T.Text)])
  get "/health" $ json (object ["status" .= ("ok" :: T.Text)])
  recipeRoutes db
  notFound $ do
    status status404
    json (object ["error" .= ("Route not found" :: T.Text)])
