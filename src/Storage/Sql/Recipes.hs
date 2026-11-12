{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Recipes
  ( listRecipes, getRecipe, createRecipe, updateRecipe, deleteRecipe, recipeBatch ) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

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
