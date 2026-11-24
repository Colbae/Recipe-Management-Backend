{-# LANGUAGE OverloadedStrings #-}

module Domain.MenuPlan
  ( MenuPlan,
    menuPlanId,
    userId,
    startDate,
    endDate,
    mkMenuPlan,
    MenuPlanInput,
    mkMenuPlanInput,
    validateMenuPlanJSON,
    inputStartDate,
    inputEndDate,
    MenuPlanEntry (..),
    MenuPlanError (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Domain.Recipe (Recipe)
import Domain.User (CurrentUser, currentUserId)
import Domain.Validation (Validation, validationResult, andThen, fromEither, checkedJSON, field, invalid)

data MenuPlan = MenuPlan Int64 Int64 MenuPlanInput
  deriving (Eq, Show)

mkMenuPlan :: Int64 -> CurrentUser -> MenuPlanInput -> MenuPlan
mkMenuPlan pid current = MenuPlan pid (currentUserId current)

menuPlanId :: MenuPlan -> Int64
menuPlanId (MenuPlan pid _ _) = pid

startDate :: MenuPlan -> Day
startDate (MenuPlan _ _ input) = inputStartDate input

endDate :: MenuPlan -> Day
endDate (MenuPlan _ _ input) = inputEndDate input

userId :: MenuPlan -> Int64
userId (MenuPlan _ uid _) = uid

instance ToJSON MenuPlan where
  toJSON plan =
    object
      [ "id" .= menuPlanId plan,
        "userId" .= userId plan,
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
  = UserNotFound
  | MenuPlanNotFound
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
  parseJSON = checkedJSON . validateMenuPlanJSON

validateMenuPlanJSON :: Value -> Either [Text] MenuPlanInput
validateMenuPlanJSON = validationResult . menuPlanJSON

menuPlanJSON :: Value -> Validation MenuPlanInput
menuPlanJSON (Object o) =
  ((,) <$> field o "startDate" <*> field o "endDate")
    `andThen` \(firstDay, lastDay) -> fromEither (mkMenuPlanInput firstDay lastDay)
menuPlanJSON _ = invalid "Expected a menu plan object"
