{-# LANGUAGE OverloadedStrings #-}

module Storage.MenuPlans (listMenuPlans, getMenuPlan, createMenuPlan, listMenuPlanRecipes, addRecipeToPlan) where

import Data.Int (Int64)
import Data.Time.Calendar (Day)
import qualified Data.Text as T
import Database.SQLite.Simple
import Domain.MenuPlan
import qualified Storage.Recipes as Recipes
import qualified Storage.Sql.MenuPlans as Sql

-- Decode stored dates through the smart constructor too, so invalid values
-- cannot enter the domain through database reads.
fromStoredPlan :: (Int64, Day, Day) -> IO MenuPlan
fromStoredPlan (pid, firstDay, lastDay) =
  case mkMenuPlanInput firstDay lastDay of
    Left errors -> ioError (userError ("Invalid stored menu plan: " ++ T.unpack (T.intercalate "; " errors)))
    Right input -> pure (mkMenuPlan pid input)

listMenuPlans :: Connection -> IO [MenuPlan]
listMenuPlans connection = query_ connection Sql.listMenuPlans >>= mapM fromStoredPlan

getMenuPlan :: Connection -> Int64 -> IO (Maybe MenuPlan)
getMenuPlan connection pid = do
  rows <- query connection Sql.getMenuPlan (Only pid)
  case rows of
    [] -> pure Nothing
    row : _ -> Just <$> fromStoredPlan row

createMenuPlan :: Connection -> MenuPlanInput -> IO MenuPlan
createMenuPlan connection input = withTransaction connection $ do
  execute connection Sql.createMenuPlan (inputStartDate input, inputEndDate input)
  pid <- lastInsertRowId connection
  pure (mkMenuPlan pid input)

listMenuPlanRecipes :: Connection -> Int64 -> IO [MenuPlanEntry]
listMenuPlanRecipes connection pid = do
  rows <- query connection Sql.listMenuPlanRecipes (Only pid)
  mapM toEntry rows
  where
    toEntry (eid, day, rid, title, servings, ingredients, instructions) = do
      recipe <- Recipes.fromStored (rid, title, servings, ingredients, instructions)
      pure (MenuPlanEntry eid day recipe)

addRecipeToPlan :: Connection -> Int64 -> Int64 -> Day -> IO (Either MenuPlanError MenuPlanEntry)
addRecipeToPlan connection pid rid day = withTransaction connection $ do
  plan <- getMenuPlan connection pid
  case plan of
    Nothing -> pure (Left MenuPlanNotFound)
    Just existing
      | day < startDate existing || day > endDate existing -> pure (Left DayOutsidePlan)
      | otherwise -> do
          recipe <- Recipes.getRecipe connection rid
          case recipe of
            Nothing -> pure (Left RecipeNotFound)
            Just found -> do
              execute connection Sql.addRecipeToPlan (pid, rid, day)
              eid <- lastInsertRowId connection
              pure (Right (MenuPlanEntry eid day found))
