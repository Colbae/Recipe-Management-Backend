{-# LANGUAGE OverloadedStrings #-}

module Api.FoodPlans where

import Api.Profiles (ProfileSelection, requireCurrentProfile)
import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection)
import Domain.MenuPlan (MenuPlanError (..), MenuPlanInput, menuPlanId)
import Network.HTTP.Types
import qualified Storage.MenuPlans as Store
import Text.Read (readMaybe)
import Web.Scotty

menuPlanRoutes :: MVar Connection -> ProfileSelection -> ScottyM ()
menuPlanRoutes db profiles = do
  get "/menuPlans" $ do
    current <- requireCurrentProfile db profiles
    runDb (`Store.listMenuPlans` current) >>= json
  get "/menuPlans/:id" $ do
    current <- requireCurrentProfile db profiles
    mId <- readId
    runDb (\connection -> Store.getMenuPlan connection current mId) >>= maybe missing json
  get "/menuPlans/:id/recipes" $ do
    current <- requireCurrentProfile db profiles
    mId <- readId
    runDb (\connection -> Store.listMenuPlanRecipes connection current mId) >>= maybe missing json
  post "/menuPlans/:id/recipes" $ do
    current <- requireCurrentProfile db profiles
    mId <- readId
    AddRecipeInput rid day <- readAddRecipe
    result <- runDb (\connection -> Store.addRecipeToPlan connection current mId rid day)
    case result of
      Right saved -> status status201 >> json saved
      Left err -> rejectMenuError err
  post "/menuPlans" $ do
    current <- requireCurrentProfile db profiles
    m <- readMenuPlan
    result <- runDb (\connection -> Store.createMenuPlan connection current m)
    case result of
      Left err -> rejectMenuError err
      Right saved -> do
        status status201
        setHeader "Location" (TL.pack ("/menuPlans/" ++ show (menuPlanId saved)))
        json saved
  where
    runDb action = liftIO (withMVar db action)

rejectMenuError :: MenuPlanError -> ActionM a
rejectMenuError UserNotFound = rejectEntry status404 "User not found"
rejectMenuError MenuPlanNotFound = rejectEntry status404 "Menu plan not found"
rejectMenuError RecipeNotFound = rejectEntry status404 "Recipe not found"
rejectMenuError DayOutsidePlan = rejectEntry status400 "day must be within the menu plan date range"

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
