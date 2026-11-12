module Domain.IngredientsFilter (recipeIngredients, filterRecipes) where

import qualified Data.Text as T
import Domain.Recipe
import Domain.Units (convertUnits)

recipeIngredients :: Recipe -> [Ingredient]
recipeIngredients = ingredients . recipeData

filterRecipes :: [Ingredient] -> [Recipe] -> [Recipe]
filterRecipes onHand = filter canMake
  where
    canMake recipe =
      let required = recipeIngredients recipe
       in all (hasEnough required) required

    hasEnough required ingredient =
      let availableAmount = totalInUnit ingredient onHand
          requiredAmount = totalInUnit ingredient required
       in availableAmount >= requiredAmount
            || abs (availableAmount - requiredAmount)
              <= 1e-9 * max (abs availableAmount) (abs requiredAmount) -- floating point err
    totalInUnit target candidates =
      sum
        [ converted
          | candidate <- candidates,
            normalizedName candidate == normalizedName target,
            Just converted <- [convertUnits (unit candidate) (unit target) (amount candidate)]
        ]

    normalizedName = T.toCaseFold . T.strip . name
