{-# LANGUAGE OverloadedStrings #-}

module Domain.MenuPlan
  ( MenuPlan,
    menuPlanId,
    startDate,
    endDate,
    mkMenuPlan,
    MenuPlanInput,
    mkMenuPlanInput,
    inputStartDate,
    inputEndDate,
    MenuPlanEntry (..),
    MenuPlanError (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Domain.Recipe (Recipe)

data MenuPlan = MenuPlan Int64 MenuPlanInput
  deriving (Eq, Show)

mkMenuPlan :: Int64 -> MenuPlanInput -> MenuPlan
mkMenuPlan = MenuPlan

menuPlanId :: MenuPlan -> Int64
menuPlanId (MenuPlan pid _) = pid

startDate :: MenuPlan -> Day
startDate (MenuPlan _ input) = inputStartDate input

endDate :: MenuPlan -> Day
endDate (MenuPlan _ input) = inputEndDate input

instance ToJSON MenuPlan where
  toJSON plan =
    object
      [ "id" .= menuPlanId plan,
        "startDate" .= startDate plan,
        "endDate" .= endDate plan
      ]

data MenuPlanEntry = MenuPlanEntry
  { entryId :: Int64,
    entryDay :: Day,
    entryRecipe :: Recipe
  }
  deriving (Eq, Show)

instance ToJSON MenuPlanEntry where
  toJSON entry =
    object
      [ "id" .= entryId entry,
        "day" .= entryDay entry,
        "recipe" .= entryRecipe entry
      ]

data MenuPlanError
  = MenuPlanNotFound
  | RecipeNotFound
  | DayOutsidePlan
  deriving (Eq, Show)

data MenuPlanInput = MenuPlanInput Day Day
  deriving (Eq, Show)

mkMenuPlanInput :: Day -> Day -> Either [Text] MenuPlanInput
mkMenuPlanInput firstDay lastDay
  | firstDay > lastDay = Left ["startDate must be on or before endDate"]
  | otherwise = Right (MenuPlanInput firstDay lastDay)

-- Generated getters not exported to make invalid menu plans impossible
inputStartDate :: MenuPlanInput -> Day
inputStartDate (MenuPlanInput firstDay _) = firstDay

inputEndDate :: MenuPlanInput -> Day
inputEndDate (MenuPlanInput _ lastDay) = lastDay

instance FromJSON MenuPlanInput where
  parseJSON = withObject "MenuPlanInput" $ \o -> do
    firstDay <- o .: "startDate"
    lastDay <- o .: "endDate"
    either
      (fail . T.unpack . T.intercalate "; ")
      pure
      (mkMenuPlanInput firstDay lastDay)
