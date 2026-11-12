{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.MenuPlans
  ( addRecipeToPlan, createMenuPlan, getMenuPlan, listMenuPlans, listMenuPlanRecipes ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

addRecipeToPlan :: Query
addRecipeToPlan = fromString $(makeRelativeToProject "sql/menu_plans/add_recipe.sql" >>= embedStringFile)

createMenuPlan :: Query
createMenuPlan = fromString $(makeRelativeToProject "sql/menu_plans/create_plan.sql" >>= embedStringFile)

getMenuPlan :: Query
getMenuPlan = fromString $(makeRelativeToProject "sql/menu_plans/get_plan.sql" >>= embedStringFile)

listMenuPlans :: Query
listMenuPlans = fromString $(makeRelativeToProject "sql/menu_plans/list_plans.sql" >>= embedStringFile)

listMenuPlanRecipes :: Query
listMenuPlanRecipes = fromString $(makeRelativeToProject "sql/menu_plans/list_recipes.sql" >>= embedStringFile)
