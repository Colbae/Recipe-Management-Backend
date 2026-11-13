{-# LANGUAGE TemplateHaskell #-}

module Storage.Sql.Pantries (createPantry, updateIngredients, getPantry) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.String (fromString)
import Database.SQLite.Simple (Query)

createPantry :: Query
createPantry = fromString $(makeRelativeToProject "sql/pantry/create_pantry.sql" >>= embedStringFile)

updateIngredients :: Query
updateIngredients = fromString $(makeRelativeToProject "sql/pantry/update_ingredients.sql" >>= embedStringFile)

getPantry :: Query
getPantry = fromString $(makeRelativeToProject "sql/pantry/get_pantry.sql" >>= embedStringFile)
