{-# LANGUAGE OverloadedStrings #-}

module Api.Pantry (pantryRoutes) where

import Api.Helpers (readNumberParam, readPantry, readParam, rejectEntry, runDb)
import Api.Profiles (ProfileSelection, requireCurrentProfile)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (object, (.=))
import Data.Int (Int64)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.Ingredients (combineIngredients)
import Domain.MenuPlan (MenuPlanEntry (entryRecipe))
import Domain.Pantry (PantryError (UserNotFound), ingredientsList, pantryId, subtractIngredients)
import Domain.Recipe (extractIngredientsFromList, ingredients, recipeData)
import Network.HTTP.Types
import qualified Storage.MenuPlans as MenuPlanStore
import qualified Storage.Pantry as PantryStore
import qualified Storage.Recipes as RecipeStore
import Web.Scotty

pantryRoutes :: MVar Connection -> ProfileSelection -> ScottyM ()
pantryRoutes db profiles = do
  get "/pantry/:id" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    runDb db (\connection -> PantryStore.getPantry connection current pId)
      >>= maybe (rejectEntry status404 "Pantry not found") json
  get "/pantry/:id/recipes" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    after <- readNumberParam "after" 0 0 (maxBound :: Int64)
    limit <- readNumberParam "limit" 20 1 100
    result <- runDb db $ \connection -> do
      pantry <- PantryStore.getPantry connection current pId
      case pantry of
        Nothing -> pure Nothing
        Just stored ->
          Just
            <$> RecipeStore.filterRecipesPage
              connection
              (ingredientsList stored)
              after
              (fromIntegral limit)
    case result of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just (recipes, nextAfter) ->
        json (object ["recipes" .= recipes, "nextAfter" .= nextAfter])
  get "/pantry/:id/recipes/:recipeId/shoppingList" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    rId <- readParam "recipeId"
    pResults <- runDb db (\connection -> PantryStore.getPantry connection current pId)
    case pResults of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> do
        rResults <- runDb db (`RecipeStore.getRecipe` rId)
        case rResults of
          Nothing -> rejectEntry status404 "Recipe not found"
          Just recipe -> do
            case subtractIngredients
              (ingredients (recipeData recipe))
              (ingredientsList pantry) of
              Left _ ->
                rejectEntry status500 "Could not calculate shopping list"
              Right items ->
                json items
  get "/pantry/:id/foodPlans/:menuId/shoppingList" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    mId <- readParam "menuId"
    pResults <- runDb db $ \connection ->
      PantryStore.getPantry connection current pId
    case pResults of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> do
        mResults <- runDb db (\connection -> MenuPlanStore.listMenuPlanRecipes connection current mId)
        case mResults of
          Nothing -> rejectEntry status404 "Menu plan not found"
          Just entries -> do
            let required =
                  extractIngredientsFromList (map entryRecipe entries)
            case combineIngredients required >>= (`subtractIngredients` ingredientsList pantry) of
              Left _ ->
                rejectEntry status500 "Could not calculate shopping list"
              Right items ->
                json items
  post "/pantry" $ do
    current <- requireCurrentProfile db profiles
    p <- readPantry
    results <- runDb db (\connection -> PantryStore.createPantry connection current p)
    case results of
      Left err -> rejectPantryError err
      Right pantry -> do
        status status201
        setHeader "Location" (TL.pack ("/pantry/" ++ show (pantryId pantry)))
        json pantry
  post "/pantry/:id/add" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    p <- readPantry
    results <- runDb db (\connection -> PantryStore.addIngredients connection current pId p)
    case results of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> json pantry
  post "/pantry/:id/remove" $ do
    current <- requireCurrentProfile db profiles
    pId <- readParam "id"
    p <- readPantry
    results <- runDb db (\connection -> PantryStore.removeIngredients connection current pId p)
    case results of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> json pantry

rejectPantryError :: PantryError -> ActionM a
rejectPantryError UserNotFound = rejectEntry status404 "User not found"
