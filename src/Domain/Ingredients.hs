module Domain.Ingredients (combineIngredients) where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Recipe (Ingredient)
import qualified Domain.Recipe as Recipe
import Domain.Units (convertUnits)

combineIngredients :: [Ingredient] -> Either [Text] [Ingredient]
combineIngredients = foldM add []
  where
    add [] incoming = Right [incoming]
    add (existing : rest) incoming
      | sameFood existing incoming,
        Just converted <- convertUnits (Recipe.unit incoming) (Recipe.unit existing) (Recipe.amount incoming) = do
          combined <-
            Recipe.mkIngredients
              (Recipe.name existing)
              (Recipe.amount existing + converted)
              (Recipe.unit existing)
              (Recipe.food existing)
          pure (combined : rest)
      | otherwise = (existing :) <$> add rest incoming

    sameFood a b = case (Recipe.food a, Recipe.food b) of
      (Just first, Just second) -> first == second
      (Nothing, Nothing) -> normalizedName a == normalizedName b
      _ -> False

    normalizedName = T.toCaseFold . T.strip . Recipe.name
