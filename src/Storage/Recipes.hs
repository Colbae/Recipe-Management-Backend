{-# LANGUAGE OverloadedStrings #-}

module Storage.Recipes (fromStored, listRecipes, filterRecipesPage, getRecipe, createRecipe, updateRecipe, deleteRecipe) where

import Data.Aeson (ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Domain.IngredientsFilter (filterRecipes)
import Domain.Recipe
import Domain.User (CurrentUser, currentUserId)
import qualified Storage.Sql as Sql
import qualified Storage.Users as Users

jsonText :: (ToJSON a) => a -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

fromStored :: (Int64, Int64, Text, Int, Text, Text) -> IO Recipe
fromStored (rid, aid, t, s, ing, steps) = do
  is <- decodeStored ing
  ss <- decodeStored steps
  case mkRecipeInput t s is ss of
    Left errors -> ioError (userError ("Invalid stored recipe: " ++ T.unpack (T.intercalate "; " errors)))
    Right input -> case mkRecipeCreateInput aid input of
      Left errors -> ioError (userError ("Invalid stored author: " ++ T.unpack (T.intercalate "; " errors)))
      Right creation -> pure (mkRecipe rid creation)
  where
    decodeStored v =
      either
        (ioError . userError . ("Invalid stored recipe JSON: " ++))
        pure
        (eitherDecodeStrict' (TE.encodeUtf8 v))

listRecipes :: Connection -> IO [Recipe]
listRecipes c = query_ c Sql.listRecipes >>= mapM fromStored

getRecipe :: Connection -> Int64 -> IO (Maybe Recipe)
getRecipe c rid = do
  rows <- query c Sql.getRecipe (Only rid)
  case rows of
    [] -> pure Nothing
    row : _ -> Just <$> fromStored row

createRecipe :: Connection -> RecipeCreateInput -> IO (Either RecipeError Recipe)
createRecipe c input = withTransaction c $ do
  let aid = createAuthorId input
      r = createRecipeData input
  exists <- Users.userExists c aid
  if not exists
    then pure (Left AuthorNotFound)
    else do
      execute
        c
        Sql.createRecipe
        (title r, servings r, jsonText (ingredients r), jsonText (instructions r), aid)
      rid <- lastInsertRowId c
      pure (Right (mkRecipe rid input))

updateRecipe :: Connection -> CurrentUser -> Int64 -> RecipeInput -> IO (Maybe Recipe)
updateRecipe c current rid r = withTransaction c $ do
  execute
    c
    Sql.updateRecipe
    (title r, servings r, jsonText (ingredients r), jsonText (instructions r), rid, currentUserId current)
  count <- changes c
  if count == 0 then pure Nothing else getRecipe c rid

deleteRecipe :: Connection -> CurrentUser -> Int64 -> IO Bool
deleteRecipe c current rid = do
  execute c Sql.deleteRecipe (rid, currentUserId current)
  (> 0) <$> changes c

filterRecipesPage :: Connection -> [Ingredient] -> Int64 -> Int -> IO ([Recipe], Maybe Int64)
filterRecipesPage c onHand after pageSize
  | after < 0 || pageSize < 1 || pageSize > 100 =
      ioError (userError "Invalid recipe filter cursor or page size")
  | otherwise = withTransaction c $ do
      matches <- collect after []
      let page = take pageSize matches
          nextAfter = if length matches > pageSize then Just (recipeId (last page)) else Nothing
      pure (page, nextAfter)
  where
    batchSize = 200 :: Int
    wanted = pageSize + 1

    collect cursor found = do
      rows <- query c Sql.recipeBatch (cursor, batchSize)
      batch <- mapM fromStored rows
      let matching = take (wanted - length found) (filterRecipes onHand batch)
          foundNow = reverse matching ++ found
      if length foundNow == wanted || length batch < batchSize
        then pure (reverse foundNow)
        else collect (recipeId (last batch)) foundNow
