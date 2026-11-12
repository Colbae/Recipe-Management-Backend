-- Re-export the query groups so existing imports of Storage.Sql keep working.
module Storage.Sql
  ( module Storage.Sql.Recipes
  , module Storage.Sql.MenuPlans
  , module Storage.Sql.Migrations
  , module Storage.Sql.Connection
  ) where

import Storage.Sql.Recipes
import Storage.Sql.MenuPlans
import Storage.Sql.Migrations
import Storage.Sql.Connection
