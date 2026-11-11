{-# LANGUAGE OverloadedStrings #-}

module Api.Recipes (recipeRoutes) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson (eitherDecode, object, (.=))
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.Recipe
import Network.HTTP.Types.Status
import qualified Storage.Recipes as Store
import Text.Read (readMaybe)
import Web.Scotty

recipeRoutes :: MVar Connection -> ScottyM ()
recipeRoutes db = do
  get "/recipes" $ runDb Store.listRecipes >>= json
  get "/recipes/:id" $ do
    rid <- readId
    runDb (`Store.getRecipe` rid) >>= maybe missing json
  post "/recipes" $ do
    r <- readRecipe
    saved <- runDb (`Store.createRecipe` r)
    status status201
    setHeader "Location" (TL.pack ("/recipes/" ++ show (recipeId saved)))
    json saved
  put "/recipes/:id" $ do
    rid <- readId
    r <- readRecipe
    runDb (\c -> Store.updateRecipe c rid r) >>= maybe missing json
  delete "/recipes/:id" $ do
    rid <- readId
    removed <- runDb (`Store.deleteRecipe` rid)
    if removed then status status204 >> raw "" else missing
  where
    runDb action = liftIO (withMVar db action)

missing :: ActionM ()
missing = do
  status status404
  json (object ["error" .= ("Recipe not found" :: T.Text)])

readId :: ActionM Int64
readId = do
  value <- pathParam "id" :: ActionM T.Text
  case readMaybe (T.unpack value) of
    Just rid | rid > 0 -> pure rid
    _ -> do
      status status400
      json (object ["error" .= ("id must be a positive integer" :: T.Text)])
      finish

readRecipe :: ActionM RecipeInput
readRecipe = do
  payload <- body
  case eitherDecode payload of
    Left _ -> reject ["Invalid recipe JSON: expected title, servings, ingredients and instructions"]
    Right r -> case validateRecipe r of
      [] -> pure r
      errors -> reject errors
  where
    reject :: [T.Text] -> ActionM a
    reject errors = do
      status status400
      json (object ["errors" .= errors])
      finish
