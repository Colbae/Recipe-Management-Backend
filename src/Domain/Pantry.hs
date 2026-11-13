{-# LANGUAGE OverloadedStrings #-}

module Domain.Pantry
  ( Pantry,
    pantryId,
    pantryData,
    mkPantry,
    PantryInput (..),
    userId,
    subtractIngredients,
    PantryError (..),
    ingredientsList,
  )
where

import Control.Monad (foldM)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Recipe (Ingredient)
import qualified Domain.Recipe as Recipe
import Domain.Units (convertUnits)
import Domain.User (CurrentUser, currentUserId)

data Pantry = Pantry Int64 CurrentUser PantryInput
  deriving (Eq, Show)

mkPantry :: Int64 -> CurrentUser -> PantryInput -> Pantry
mkPantry = Pantry

pantryId :: Pantry -> Int64
pantryId (Pantry pid _ _) = pid

pantryData :: Pantry -> PantryInput
pantryData (Pantry _ _ input) = input

ingredientsList :: Pantry -> [Ingredient]
ingredientsList (Pantry _ _ (PantryInput ing)) = ing

userId :: Pantry -> Int64
userId (Pantry _ current _) = currentUserId current

instance ToJSON Pantry where
  toJSON pantry =
    object
      [ "id" .= pantryId pantry,
        "userId" .= userId pantry,
        "ingredients" .= ingredientsList pantry
      ]

-- Simple alias, since all other pantry data comes from elsewhere
newtype PantryInput = PantryInput [Ingredient]
  deriving (Eq, Show)

instance ToJSON PantryInput where
  toJSON (PantryInput items) = object ["ingredients" .= items]

instance FromJSON PantryInput where
  parseJSON = withObject "PantryInput" $ \o -> do
    PantryInput <$> o .: "ingredients"

data PantryError = UserNotFound
  deriving (Eq, Show)

subtractIngredients :: [Ingredient] -> [Ingredient] -> Either [Text] [Ingredient]
subtractIngredients = foldM subtractOne
  where
    subtractOne stock requested = consume (Recipe.amount requested) stock
      where
        consume _ [] = Right []
        consume remaining (item : rest)
          | normalizedName item /= normalizedName requested =
              (item :) <$> consume remaining rest
          | otherwise = case convertUnits (Recipe.unit requested) (Recipe.unit item) remaining of
              Nothing -> (item :) <$> consume remaining rest
              Just needed
                | close needed (Recipe.amount item) -> Right rest
                | needed < Recipe.amount item -> do
                    updated <-
                      Recipe.mkIngredients
                        (Recipe.name item)
                        (Recipe.amount item - needed)
                        (Recipe.unit item)
                        (Recipe.food item)
                    Right (updated : rest)
                | otherwise -> case convertUnits (Recipe.unit item) (Recipe.unit requested) (Recipe.amount item) of
                    Nothing -> (item :) <$> consume remaining rest
                    Just consumed -> consume (max 0 (remaining - consumed)) rest

    normalizedName = T.toCaseFold . T.strip . Recipe.name
    close a b =
      not (isInfinite a || isNaN a)
        && abs (a - b) <= 1e-9 * max (abs a) (abs b)
