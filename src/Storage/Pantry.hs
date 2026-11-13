{-# LANGUAGE OverloadedStrings #-}

module Storage.Pantry (createPantry, getPantry, addIngredients, removeIngredients) where

import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Domain.Pantry
import Domain.Recipe (Ingredient)
import Domain.User (CurrentUser, currentUserId, mkCurrentUser)
import qualified Storage.Sql.Pantries as Sql
import qualified Storage.Users as Users

fromStoredPantry :: (Int64, Int64, Text) -> IO Pantry
fromStoredPantry (pid, uid, stored) = do
  items <-
    either
      (ioError . userError . ("Invalid stored pantry JSON: " ++))
      pure
      (eitherDecodeStrict' (TE.encodeUtf8 stored))
  owner <- maybe (ioError (userError "Invalid stored pantry owner")) pure (mkCurrentUser uid)
  pure (mkPantry pid owner (PantryInput items))

ingredientsJSON :: [Ingredient] -> Text
ingredientsJSON = TE.decodeUtf8 . BL.toStrict . encode

createPantry :: Connection -> CurrentUser -> [Ingredient] -> IO (Either PantryError Pantry)
createPantry connection current items = withTransaction connection $ do
  let uid = currentUserId current
  exists <- Users.userExists connection uid
  if not exists
    then pure (Left UserNotFound)
    else do
      execute connection Sql.createPantry (uid, ingredientsJSON items)
      pid <- lastInsertRowId connection
      pure (Right (mkPantry pid current (PantryInput items)))

getPantry :: Connection -> CurrentUser -> Int64 -> IO (Maybe Pantry)
getPantry connection current pid = do
  rows <- query connection Sql.getPantry (pid, currentUserId current)
  case rows of
    [] -> pure Nothing
    row : _ -> Just <$> fromStoredPantry row

addIngredients :: Connection -> CurrentUser -> Int64 -> [Ingredient] -> IO (Maybe Pantry)
addIngredients connection current pid added = withImmediateTransaction connection $ do
  existing <- getPantry connection current pid
  case existing of
    Nothing -> pure Nothing
    Just pantry -> do
      let combined = ingredientsList pantry ++ added
      execute
        connection
        Sql.updateIngredients
        (ingredientsJSON combined, pid, currentUserId current)
      getPantry connection current pid

removeIngredients :: Connection -> CurrentUser -> Int64 -> [Ingredient] -> IO (Maybe Pantry)
removeIngredients connection current pid removed = withImmediateTransaction connection $ do
  existing <- getPantry connection current pid
  case existing of
    Nothing -> pure Nothing
    Just pantry -> do
      difference <-
        either
          (ioError . userError . T.unpack . T.intercalate "; ")
          pure
          (subtractIngredients (ingredientsList pantry) removed)
      execute
        connection
        Sql.updateIngredients
        (ingredientsJSON difference, pid, currentUserId current)
      getPantry connection current pid
