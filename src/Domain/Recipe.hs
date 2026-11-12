{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Recipe (Ingredient (..), RecipeInput (..), Recipe (..), validateRecipe, validateIngredient) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Food (FoodReference (..))
import Domain.Units (Unit)
import GHC.Generics (Generic)

data Ingredient = Ingredient
  { name :: Text,
    amount :: Double,
    unit :: Unit,
    food :: Maybe FoodReference
  }
  deriving (Eq, Show, Generic)

instance FromJSON Ingredient

instance ToJSON Ingredient where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data RecipeInput = RecipeInput
  { title :: Text,
    servings :: Int,
    ingredients :: [Ingredient],
    instructions :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON RecipeInput

instance ToJSON RecipeInput

data Recipe = Recipe
  { recipeId :: Int64,
    recipeData :: RecipeInput
  }
  deriving (Eq, Show)

instance ToJSON Recipe where
  toJSON (Recipe rid r) =
    object
      [ "id" .= rid,
        "title" .= title r,
        "servings" .= servings r,
        "ingredients" .= ingredients r,
        "instructions" .= instructions r
      ]

validateRecipe :: RecipeInput -> [Text]
validateRecipe r =
  ["title must not be blank" | blank (title r)]
    ++ ["servings must be positive" | servings r <= 0]
    ++ ["ingredients must not be empty" | null (ingredients r)]
    ++ ["instructions must contain non-blank steps" | null (instructions r) || any blank (instructions r)]
    ++ concatMap validateIngredient (ingredients r)
  where
    blank = T.null . T.strip

validateIngredient :: Ingredient -> [Text]
validateIngredient i =
  ["ingredient name must not be blank" | T.null (T.strip (name i))]
    ++ ["ingredient amount must be finite and positive" | amount i <= 0 || isNaN (amount i) || isInfinite (amount i)]
    ++ case food i of
      Nothing -> []
      Just ref ->
        ["ingredient food provider must be usda" | provider ref /= "usda"] -- only usda supported at the moment
          ++ ["ingredient foodId must be positive" | foodId ref <= 0]
