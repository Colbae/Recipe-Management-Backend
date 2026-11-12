{-# LANGUAGE OverloadedStrings #-}

module Api.FoodPlans where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson
import Data.Int (Int64)
import Data.Time.Calendar (Day)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.MenuPlan (MenuPlanInput, MenuPlanError (..), menuPlanId)
import Network.HTTP.Types
import qualified Storage.MenuPlans as Store
import Text.Read (readMaybe)
import Web.Scotty

menuPlanRoutes :: MVar Connection -> ScottyM ()
menuPlanRoutes db = do
  get "/menuPlans" $ runDb Store.listMenuPlans >>= json
  get "/menuPlans/:id" $ do
    mId <- readId
    runDb (`Store.getMenuPlan` mId) >>= maybe missing json
  get "/menuPlans/:id/recipes" $ do
    mId <- readId
    runDb (`Store.listMenuPlanRecipes` mId) >>= json
  post "/menuPlans/:id/recipes" $ do
    mId <- readId
    AddRecipeInput rid day <- readAddRecipe
    result <- runDb (\connection -> Store.addRecipeToPlan connection mId rid day)
    case result of
      Right saved -> status status201 >> json saved
      Left MenuPlanNotFound -> rejectEntry status404 "Menu plan not found"
      Left RecipeNotFound -> rejectEntry status404 "Recipe not found"
      Left DayOutsidePlan -> rejectEntry status400 "day must be within the menu plan date range"
  post "/menuPlans" $ do
    m <- readMenuPlan
    saved <- runDb (`Store.createMenuPlan` m)
    status status201
    setHeader "Location" (TL.pack ("/menuPlans/" ++ show (menuPlanId saved)))
    json saved
  where
    runDb action = liftIO (withMVar db action)

data AddRecipeInput = AddRecipeInput Int64 Day

instance FromJSON AddRecipeInput where
  parseJSON = withObject "AddRecipeInput" $ \o ->
    AddRecipeInput <$> o .: "recipeId" <*> o .: "day"

readAddRecipe :: ActionM AddRecipeInput
readAddRecipe = do
  payload <- body
  case eitherDecode payload of
    Left _ -> rejectEntry status400 "Expected recipeId (integer) and day (YYYY-MM-DD)"
    Right input@(AddRecipeInput rid _)
      | rid > 0 -> pure input
      | otherwise -> rejectEntry status400 "recipeId must be a positive integer"

rejectEntry :: Status -> T.Text -> ActionM a
rejectEntry code message = do
  status code
  json (object ["error" .= message])
  finish

readMenuPlan :: ActionM MenuPlanInput
readMenuPlan = do
  payload <- body
  case eitherDecode payload of
    Left errorMessage -> reject [T.pack errorMessage]
    Right m -> pure m
  where
    reject :: [T.Text] -> ActionM a
    reject errors = do
      status status400
      json (object ["errors" .= errors])
      finish

missing :: ActionM ()
missing = do
  status status404
  json (object ["error" .= ("Menu plan not found" :: T.Text)])

readId :: ActionM Int64
readId = do
  value <- pathParam "id" :: ActionM T.Text
  case readMaybe (T.unpack value) of
    Just rid | rid > 0 -> pure rid
    _ -> do
      status status400
      json (object ["error" .= ("id must be a positive integer" :: T.Text)])
      finish
