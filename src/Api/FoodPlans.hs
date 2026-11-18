{-# LANGUAGE OverloadedStrings #-}

module Api.FoodPlans where

import Api.Profiles (ProfileSelection, requireCurrentProfile)
import Api.Helpers (nutritionReportResult, readPantry, readParam, rejectEntry, requireClient, runDb)
import Control.Concurrent.MVar (MVar)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar (Day)
import Database.SQLite.Simple (Connection)
import Domain.MenuPlan (MenuPlanEntry (entryRecipe), MenuPlanError (..), MenuPlanInput, menuPlanId)
import Domain.Pantry (subtractIngredients)
import Domain.Recipe (extractIngredientsFromList, extractNormalizedIngredientsFromList)
import Integration.Usda (UsdaClient, generateNutritionReport)
import Network.HTTP.Types
import qualified Storage.MenuPlans as Store
import Web.Scotty

menuPlanRoutes :: MVar Connection -> ProfileSelection -> Maybe UsdaClient -> ScottyM ()
menuPlanRoutes db profiles configured = do
  get "/menuPlans" $ do
    current <- requireCurrentProfile db profiles
    runDb db (`Store.listMenuPlans` current) >>= json
  get "/menuPlans/:id" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    runDb db (\connection -> Store.getMenuPlan connection current mId) >>= maybe missing json
  get "/menuPlans/:id/recipes" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    runDb db (\connection -> Store.listMenuPlanRecipes connection current mId) >>= maybe missing json
  get "/menuPlans/:id/nutritionReport" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    result <- runDb db (\connection -> Store.listMenuPlanRecipes connection current mId)
    case result of
      Nothing -> missing
      Just recipes -> do
        client <- requireClient configured
        report <-
          liftIO $
            generateNutritionReport client $
              extractNormalizedIngredientsFromList $
                map entryRecipe recipes
        nutritionReportResult report
  post "/menuPlans/:id/shoppingList" $ do
    current <- requireCurrentProfile db profiles
    onHand <- readPantry
    mId <- readParam "id"
    result <- runDb db (\connection -> Store.listMenuPlanRecipes connection current mId)
    case result of
      Nothing -> missing
      Just recipes -> do
        let ingredients = extractIngredientsFromList $ map entryRecipe recipes
        case subtractIngredients ingredients onHand of
          Left _ ->
            rejectEntry status500 "Could not calculate shopping list"
          Right items ->
            json items
  post "/menuPlans/:id/recipes" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    AddRecipeInput rid day <- readAddRecipe
    result <- runDb db (\connection -> Store.addRecipeToPlan connection current mId rid day)
    case result of
      Right saved -> status status201 >> json saved
      Left err -> rejectMenuError err
  post "/menuPlans" $ do
    current <- requireCurrentProfile db profiles
    m <- readMenuPlan
    result <- runDb db (\connection -> Store.createMenuPlan connection current m)
    case result of
      Left err -> rejectMenuError err
      Right saved -> do
        status status201
        setHeader "Location" (TL.pack ("/menuPlans/" ++ show (menuPlanId saved)))
        json saved
  put "/menuPlans/:id" $ do
    current <- requireCurrentProfile db profiles
    m <- readMenuPlan
    mId <- readParam "id"
    result <- runDb db (\connection -> Store.updatePlan connection current mId m)
    either rejectMenuError json result
  delete "/menuPlans/:id" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    removed <- runDb db (\connection -> Store.deletePlan connection current mId)
    if removed then status status204 >> raw "" else missing
  delete "/menuPlans/:id/recipes/:bridgeId" $ do
    current <- requireCurrentProfile db profiles
    mId <- readParam "id"
    bId <- readParam "bridgeId"
    removed <- runDb db (\connection -> Store.removeRecipe connection current mId bId)
    if removed then status status204 >> raw "" else rejectEntry status404 "Menu plan entry not found"

rejectMenuError :: MenuPlanError -> ActionM a
rejectMenuError UserNotFound = rejectEntry status404 "User not found"
rejectMenuError MenuPlanNotFound = rejectEntry status404 "Menu plan not found"
rejectMenuError RecipeNotFound = rejectEntry status404 "Recipe not found"
rejectMenuError DayOutsidePlan = rejectEntry status400 "All recipe entry days must be within the menu plan date range"

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
