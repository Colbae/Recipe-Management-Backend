{-# LANGUAGE OverloadedStrings #-}

module Api.Recipes (recipeRoutes) where

import Api.Helpers
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (..), eitherDecode, object, (.=))
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
  get "/recipes" $ runDb db Store.listRecipes >>= json
  post "/recipes/filter" $ do
    onHand <- readPantry
    after <- readNumberParam "after" 0 0 (maxBound :: Int64) -- To paginate the results
    limit <- readNumberParam "limit" 20 1 100
    (recipes, nextAfter) <- runDb db (\c -> Store.filterRecipesPage c onHand after (fromIntegral limit))
    json (object ["recipes" .= recipes, "nextAfter" .= nextAfter])
  get "/recipes/:id" $ do
    rid <- readId
    runDb db (`Store.getRecipe` rid) >>= maybe missing json
  post "/recipes" $ do
    r <- readRecipe
    result <- runDb db (`Store.createRecipe` r)
    case result of
      Left AuthorNotFound -> do
        status status404
        json (object ["error" .= ("Author not found" :: T.Text)])
      Right saved -> do
        status status201
        setHeader "Location" (TL.pack ("/recipes/" ++ show (recipeId saved)))
        json saved

missing :: ActionM ()
missing = do
  status status404
  json (object ["error" .= ("Recipe not found" :: T.Text)])

readRecipe :: (FromJSON a) => ActionM a
readRecipe = do
  payload <- body
  case eitherDecode payload of
    Left errorMessage -> reject [T.pack errorMessage]
    Right r -> pure r
  where
    reject :: [T.Text] -> ActionM a
    reject errors = do
      status status400
      json (object ["errors" .= errors])
      finish

readNumberParam :: TL.Text -> Int64 -> Int64 -> Int64 -> ActionM Int64
readNumberParam key fallback lower upper = do
  value <- queryParamMaybe key :: ActionM (Maybe T.Text)
  case value of
    Nothing -> pure fallback
    Just rawValue -> case readMaybe (T.unpack rawValue) of
      Just n | n >= lower && n <= upper -> pure n
      _ -> do
        status status400
        json (object ["error" .= (TL.toStrict key <> " must be an integer between " <> T.pack (show lower) <> " and " <> T.pack (show upper))])
        finish
