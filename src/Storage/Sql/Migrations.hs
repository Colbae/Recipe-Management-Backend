{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Migrations (initialRecipeSchema, menuPlanSchema, usersSchema, pantrySchema, createMigrationTable, listMigrations, recordMigration) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Query)

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

usersSchema :: Text
usersSchema = T.pack $(makeRelativeToProject "sql/migrations/003_users.sql" >>= embedStringFile)

pantrySchema :: Text
pantrySchema = T.pack $(makeRelativeToProject "sql/migrations/004_pantry.sql" >>= embedStringFile)
