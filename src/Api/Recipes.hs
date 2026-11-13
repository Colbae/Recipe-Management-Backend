{-# LANGUAGE OverloadedStrings #-}

module Api.Recipes (recipeRoutes) where

import Api.Helpers
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (..), eitherDecode, object, (.=))
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.Pantry (subtractIngredients)
import Domain.Recipe
import Network.HTTP.Types.Status
import qualified Storage.Recipes as Store
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
    rid <- readParam "id"
    runDb db (`Store.getRecipe` rid) >>= maybe missing json
  post "/recipes/:id/shoppingList" $ do
    rid <- readParam "id"
    onHand <- readPantry
    result <- runDb db (`Store.getRecipe` rid)
    case result of
      Nothing -> missing
      Just recipe -> case subtractIngredients (ingredients (recipeData recipe)) onHand of
        Left _ -> rejectEntry status500 "Could not calculate shopping list"
        Right items -> json items
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
