{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql
  ( initialRecipeSchema,
    createMigrationTable,
    listMigrations,
    recordMigration,
    busyTimeout,
    listRecipes,
    recipeBatch,
    getRecipe,
    createRecipe,
    updateRecipe,
    deleteRecipe,
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Query)

-- Embed at compile time; file-embed tracks changes and triggers recompilation.
-- Parameters still use sqlite-simple's binding, never string interpolation.

initialRecipeSchema :: Text
initialRecipeSchema = T.pack $(makeRelativeToProject "sql/migrations/001_create_recipes.sql" >>= embedStringFile)

createMigrationTable :: Query
createMigrationTable = fromString $(makeRelativeToProject "sql/migration-tracking/create.sql" >>= embedStringFile)

listMigrations :: Query
listMigrations = fromString $(makeRelativeToProject "sql/migration-tracking/list.sql" >>= embedStringFile)

recordMigration :: Query
recordMigration = fromString $(makeRelativeToProject "sql/migration-tracking/record.sql" >>= embedStringFile)

busyTimeout :: Query
busyTimeout = fromString $(makeRelativeToProject "sql/busy-timeout.sql" >>= embedStringFile)

listRecipes :: Query
listRecipes = fromString $(makeRelativeToProject "sql/recipes/list.sql" >>= embedStringFile)

getRecipe :: Query
getRecipe = fromString $(makeRelativeToProject "sql/recipes/get.sql" >>= embedStringFile)

createRecipe :: Query
createRecipe = fromString $(makeRelativeToProject "sql/recipes/create.sql" >>= embedStringFile)

updateRecipe :: Query
updateRecipe = fromString $(makeRelativeToProject "sql/recipes/update.sql" >>= embedStringFile)

deleteRecipe :: Query
deleteRecipe = fromString $(makeRelativeToProject "sql/recipes/delete.sql" >>= embedStringFile)

recipeBatch :: Query
recipeBatch = fromString $(makeRelativeToProject "sql/recipes/batch.sql" >>= embedStringFile)
