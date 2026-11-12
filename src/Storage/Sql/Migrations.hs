{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Migrations
  ( initialRecipeSchema, menuPlanSchema, createMigrationTable, listMigrations, recordMigration ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)
import Data.Text (Text)
import qualified Data.Text as T

initialRecipeSchema :: Text
initialRecipeSchema = T.pack $(makeRelativeToProject "sql/migrations/001_create_recipes.sql" >>= embedStringFile)

menuPlanSchema :: Text
menuPlanSchema = T.pack $(makeRelativeToProject "sql/migrations/002_menu_plan.sql" >>= embedStringFile)

createMigrationTable :: Query
createMigrationTable = fromString $(makeRelativeToProject "sql/migration-tracking/create.sql" >>= embedStringFile)

listMigrations :: Query
listMigrations = fromString $(makeRelativeToProject "sql/migration-tracking/list.sql" >>= embedStringFile)

recordMigration :: Query
recordMigration = fromString $(makeRelativeToProject "sql/migration-tracking/record.sql" >>= embedStringFile)
