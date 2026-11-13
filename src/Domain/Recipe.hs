{-# LANGUAGE OverloadedStrings #-}

module Domain.Recipe
  ( Ingredient,
    mkIngredients,
    name,
    amount,
    unit,
    food,
    RecipeInput,
    mkRecipeInput,
    title,
    servings,
    ingredients,
    instructions,
    RecipeCreateInput,
    mkRecipeCreateInput,
    createAuthorId,
    createRecipeData,
    RecipeError (..),
    Recipe,
    authorId,
    mkRecipe,
    recipeId,
    recipeData,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Food (FoodReference (..))
import Domain.Units (Unit)

data Ingredient = Ingredient Text Double Unit (Maybe FoodReference)
  deriving (Eq, Show)

name :: Ingredient -> Text
name (Ingredient n _ _ _) = n

amount :: Ingredient -> Double
amount (Ingredient _ a _ _) = a

unit :: Ingredient -> Unit
unit (Ingredient _ _ u _) = u

food :: Ingredient -> Maybe FoodReference
food (Ingredient _ _ _ f) = f

instance FromJSON Ingredient where
  parseJSON = withObject "Ingredient" $ \o -> do
    n <- o .: "name"
    a <- o .: "amount"
    u <- o .: "unit"
    f <- o .:? "food"
    checkedJSON (mkIngredients n a u f)

instance ToJSON Ingredient where
  toJSON i =
    object
      ( ["name" .= name i, "amount" .= amount i, "unit" .= unit i]
          ++ maybe [] (\ref -> ["food" .= ref]) (food i)
      )

data RecipeInput = RecipeInput Text Int [Ingredient] [Text]
  deriving (Eq, Show)

title :: RecipeInput -> Text
title (RecipeInput t _ _ _) = t

servings :: RecipeInput -> Int
servings (RecipeInput _ s _ _) = s

ingredients :: RecipeInput -> [Ingredient]
ingredients (RecipeInput _ _ ing _) = ing

instructions :: RecipeInput -> [Text]
instructions (RecipeInput _ _ _ inst) = inst

instance FromJSON RecipeInput where
  parseJSON = withObject "RecipeInput" $ \o -> do
    t <- o .: "title"
    s <- o .: "servings"
    ing <- o .: "ingredients"
    inst <- o .: "instructions"
    checkedJSON (mkRecipeInput t s ing inst)

instance ToJSON RecipeInput where
  toJSON r =
    object
      [ "title" .= title r,
        "servings" .= servings r,
        "ingredients" .= ingredients r,
        "instructions" .= instructions r
      ]

data RecipeCreateInput = RecipeCreateInput Int64 RecipeInput
  deriving (Eq, Show)

mkRecipeCreateInput :: Int64 -> RecipeInput -> Either [Text] RecipeCreateInput
mkRecipeCreateInput aid input
  | aid <= 0 = Left ["authorId must be a positive integer"]
  | otherwise = Right (RecipeCreateInput aid input)

createAuthorId :: RecipeCreateInput -> Int64
createAuthorId (RecipeCreateInput aid _) = aid

createRecipeData :: RecipeCreateInput -> RecipeInput
createRecipeData (RecipeCreateInput _ input) = input

instance FromJSON RecipeCreateInput where
  parseJSON value =
    withObject
      "RecipeCreateInput"
      ( \o -> do
          aid <- o .: "authorId"
          input <- parseJSON value
          checkedJSON (mkRecipeCreateInput aid input)
      )
      value

data RecipeError = AuthorNotFound
  deriving (Eq, Show)

data Recipe = Recipe Int64 Int64 RecipeInput
  deriving (Eq, Show)

mkRecipe :: Int64 -> RecipeCreateInput -> Recipe
mkRecipe rid input = Recipe rid (createAuthorId input) (createRecipeData input)

recipeId :: Recipe -> Int64
recipeId (Recipe rid _ _) = rid

authorId :: Recipe -> Int64
authorId (Recipe _ aid _) = aid

recipeData :: Recipe -> RecipeInput
recipeData (Recipe _ _ r) = r

checkedJSON :: Either [Text] a -> Parser a
checkedJSON = either (fail . T.unpack . T.intercalate "; ") pure

instance ToJSON Recipe where
  toJSON (Recipe rid aid r) =
    object
      [ "id" .= rid,
        "authorId" .= aid,
        "title" .= title r,
        "servings" .= servings r,
        "ingredients" .= ingredients r,
        "instructions" .= instructions r
      ]

checkValidRecipeInput :: Text -> Int -> [Ingredient] -> [Text] -> [Text]
checkValidRecipeInput t s ing inst =
  ["title must not be blank" | blank t]
    ++ ["title must be at most 255 characters" | T.length t > 255]
    ++ ["servings must be positive" | s <= 0]
    ++ ["ingredients must not be empty" | null ing]
    ++ ["instructions must contain non-blank steps" | null inst || any blank inst]
  where
    blank = T.null . T.strip

mkRecipeInput :: Text -> Int -> [Ingredient] -> [Text] -> Either [Text] RecipeInput
mkRecipeInput t s ing inst = case checkValidRecipeInput t s ing inst of
  [] -> Right (RecipeInput t s ing inst)
  e -> Left e

checkValidIngredientInput :: Text -> Double -> Maybe FoodReference -> [Text]
checkValidIngredientInput n a f =
  ["ingredient name must not be blank" | T.null (T.strip n)]
    ++ ["ingredient amount must be finite and positive" | a <= 0 || isNaN a || isInfinite a]
    ++ case f of
      Nothing -> []
      Just ref ->
        ["ingredient food provider must be usda" | provider ref /= "usda"] -- only usda supported at the moment
          ++ ["ingredient foodId must be positive" | foodId ref <= 0]

mkIngredients :: Text -> Double -> Unit -> Maybe FoodReference -> Either [Text] Ingredient
mkIngredients n a u f = case checkValidIngredientInput n a f of
  [] -> Right (Ingredient n a u f)
  e -> Left e
