{-# LANGUAGE OverloadedStrings #-}

module Storage.MenuPlans
  ( listMenuPlans,
    getMenuPlan,
    createMenuPlan,
    listMenuPlanRecipes,
    addRecipeToPlan,
    deletePlan,
    updatePlan,
    removeRecipe,
  )
where

import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Database.SQLite.Simple
import Domain.MenuPlan
import Domain.User
import qualified Storage.Recipes as Recipes
import qualified Storage.Sql.MenuPlans as Sql
import qualified Storage.Users as Users

fromStoredPlan :: (Int64, Int64, Day, Day) -> IO MenuPlan
fromStoredPlan (pid, uid, firstDay, lastDay) = do
  owner <- maybe (ioError (userError "Invalid stored menu plan owner")) pure (mkCurrentUser uid)
  case mkMenuPlanInput firstDay lastDay of
    Left errors -> ioError (userError ("Invalid stored menu plan: " ++ T.unpack (T.intercalate "; " errors)))
    Right input -> pure (mkMenuPlan pid owner input)

listMenuPlans :: Connection -> CurrentUser -> IO [MenuPlan]
listMenuPlans connection current =
  query connection Sql.listMenuPlans (Only (currentUserId current)) >>= mapM fromStoredPlan

getMenuPlan :: Connection -> CurrentUser -> Int64 -> IO (Maybe MenuPlan)
getMenuPlan connection current pid = do
  rows <- query connection Sql.getMenuPlan (pid, currentUserId current)
  case rows of
    [] -> pure Nothing
    row : _ -> Just <$> fromStoredPlan row

createMenuPlan :: Connection -> CurrentUser -> MenuPlanInput -> IO (Either MenuPlanError MenuPlan)
createMenuPlan connection current input = withTransaction connection $ do
  exists <- Users.userExists connection (currentUserId current)
  if not exists
    then pure (Left UserNotFound)
    else do
      execute connection Sql.createMenuPlan (currentUserId current, inputStartDate input, inputEndDate input)
      pid <- lastInsertRowId connection
      pure (Right (mkMenuPlan pid current input))

listMenuPlanRecipes :: Connection -> CurrentUser -> Int64 -> IO (Maybe [MenuPlanEntry])
listMenuPlanRecipes connection current pid = withTransaction connection $ do
  plan <- getMenuPlan connection current pid
  case plan of
    Nothing -> pure Nothing
    Just _ -> do
      rows <- query connection Sql.listMenuPlanRecipes (pid, currentUserId current)
      Just <$> mapM toEntry rows
  where
    toEntry (eid, day, rid, aid, title, servings, ingredients, instructions) = do
      recipe <- Recipes.fromStored (rid, aid, title, servings, ingredients, instructions)
      pure (MenuPlanEntry eid day recipe)

addRecipeToPlan :: Connection -> CurrentUser -> Int64 -> Int64 -> Day -> IO (Either MenuPlanError MenuPlanEntry)
addRecipeToPlan connection current pid rid day = withTransaction connection $ do
  plan <- getMenuPlan connection current pid
  case plan of
    Nothing -> pure (Left MenuPlanNotFound)
    Just existing
      | day < startDate existing || day > endDate existing -> pure (Left DayOutsidePlan)
      | otherwise -> do
          recipe <- Recipes.getRecipe connection rid
          case recipe of
            Nothing -> pure (Left RecipeNotFound)
            Just found -> do
              execute connection Sql.addRecipeToPlan (rid, day, pid, currentUserId current)
              changed <- changes connection
              if changed == 0
                then pure (Left MenuPlanNotFound)
                else do
                  eid <- lastInsertRowId connection
                  pure (Right (MenuPlanEntry eid day found))

deletePlan :: Connection -> CurrentUser -> Int64 -> IO Bool
deletePlan connection current pid = do
  execute connection Sql.deletePlan (pid, currentUserId current)
  (> 0) <$> changes connection

updatePlan :: Connection -> CurrentUser -> Int64 -> MenuPlanInput -> IO (Either MenuPlanError MenuPlan)
updatePlan connection current pid input = withTransaction connection $ do
  existing <- getMenuPlan connection current pid
  case existing of
    Nothing -> pure (Left MenuPlanNotFound)
    Just _ -> do
      execute connection Sql.updatePlan
        (inputStartDate input, inputEndDate input, pid, currentUserId current,
         inputStartDate input, inputEndDate input)
      count <- changes connection
      if count == 0
        then pure (Left DayOutsidePlan)
        else pure (Right (mkMenuPlan pid current input))

removeRecipe :: Connection -> CurrentUser -> Int64 -> Int64 -> IO Bool
removeRecipe connection current pid eid = do
  execute connection Sql.removeRecipe (eid, pid, currentUserId current)
  (> 0) <$> changes connection
