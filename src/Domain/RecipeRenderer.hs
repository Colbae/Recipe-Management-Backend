{-# LANGUAGE OverloadedStrings #-}

module Domain.RecipeRenderer (renderRecipe) where

import Data.Text (Text)
import qualified Data.Text as T
import Domain.Food (FoodReference (..), Provider (USDA))
import qualified Domain.Recipe as R
import Domain.Units (unitText)

renderRecipe :: R.RecipeInput -> Either [Text] Text
renderRecipe recipe = case errors of
  [] ->
    Right $
      T.unlines $
        ["Title: " <> R.title recipe, "Servings: " <> shown (R.servings recipe), "", "Ingredients:"]
          ++ map renderIngredient (R.ingredients recipe)
          ++ ["", "Instructions:"]
          ++ R.instructions recipe
  _ -> Left errors
  where
    errors =
      textErrors "title" (R.title recipe)
        ++ concat
          [ textErrors (indexed "ingredients" i) (R.name item)
              ++ [indexed "ingredients" i <> ": name must not contain @" | T.any (== '@') (R.name item)]
            | (i, item) <- zip [0 :: Int ..] (R.ingredients recipe)
          ]
        ++ concat
          [ textErrors (indexed "instructions" i) step
              ++ [indexed "instructions" i <> ": must not begin with a section header" | isHeader step]
            | (i, step) <- zip [0 :: Int ..] (R.instructions recipe)
          ]

renderIngredient :: R.Ingredient -> Text
renderIngredient item =
  shown (R.amount item)
    <> " "
    <> unitText (R.unit item)
    <> " "
    <> R.name item
    <> maybe "" renderReference (R.food item)

renderReference :: FoodReference -> Text
renderReference (FoodReference USDA fid) = " @usda:" <> shown fid

textErrors :: Text -> Text -> [Text]
textErrors label value =
  [label <> ": must be a single line" | T.any (\c -> c == '\n' || c == '\r') value]
    ++ [label <> ": leading or trailing whitespace cannot be preserved" | T.strip value /= value]

isHeader :: Text -> Bool
isHeader value =
  let (header, suffix) = T.breakOn ":" value
   in not (T.null suffix)
        && T.toLower (T.strip header) `elem` ["title", "servings", "ingredients", "instructions"]

indexed :: Text -> Int -> Text
indexed label i = label <> "[" <> shown i <> "]"

shown :: (Show a) => a -> Text
shown = T.pack . show
