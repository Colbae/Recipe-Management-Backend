{-# LANGUAGE OverloadedStrings #-}

module Domain.Nutrition where

import qualified Control.Monad
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- Nutrient units are distinct from cooking measures.
data NutrientWeight = Gram | Milligram | Microgram
  deriving (Eq, Show)

data NutrientEnergy = Kilojoule | Kilocalorie
  deriving (Eq, Show)

data NutrienUnit
  = NutrientWeightUnit NutrientWeight
  | NutreintEnergyUnit NutrientEnergy
  | InternationalUnit
  | OtherNutrientUnit Text
  deriving (Eq, Show)

instance ToJSON NutrienUnit where
  toJSON (NutreintEnergyUnit Kilocalorie) = String "kcal"
  toJSON (NutreintEnergyUnit Kilojoule) = String "kJ"
  toJSON (NutrientWeightUnit Gram) = String "g"
  toJSON (NutrientWeightUnit Milligram) = String "mg"
  toJSON (NutrientWeightUnit Microgram) = String "µg"
  toJSON InternationalUnit = String "IU"
  toJSON (OtherNutrientUnit value) = String value

instance FromJSON NutrienUnit where
  parseJSON = withText "Nutrient unit" $ \raw -> pure $ case T.toCaseFold (T.strip raw) of
    "kcal" -> NutreintEnergyUnit Kilocalorie
    "kj" -> NutreintEnergyUnit Kilojoule
    "g" -> NutrientWeightUnit Gram
    "mg" -> NutrientWeightUnit Milligram
    "ug" -> NutrientWeightUnit Microgram
    "µg" -> NutrientWeightUnit Microgram
    "μg" -> NutrientWeightUnit Microgram
    "mcg" -> NutrientWeightUnit Microgram
    "iu" -> InternationalUnit
    _ -> OtherNutrientUnit raw

data Nutrient = Nutrient
  { nutrientId :: Int,
    nutrientName :: Text,
    nutrientUnit :: NutrienUnit,
    nutrientAmount :: Maybe Double
  }
  deriving (Eq, Show)

instance ToJSON Nutrient where
  toJSON n =
    object
      [ "id" .= nutrientId n,
        "name" .= nutrientName n,
        "unit" .= nutrientUnit n,
        "amount" .= nutrientAmount n
      ]

-- IDs are essential: protein and fat both use grams but are different nutrients.
newtype NutritionReport = NutritionReport [Nutrient]
  deriving (Eq, Show)

instance ToJSON NutritionReport where
  toJSON (NutritionReport values) =
    object
      [ "nutrients" .= values,
        "estimated" .= True
      ]

convertNutrientUnit :: NutrienUnit -> NutrienUnit -> Double -> Maybe Double
convertNutrientUnit from to value
  | from == to = Just value
convertNutrientUnit (NutrientWeightUnit from) (NutrientWeightUnit to) value =
  Just (value * (massFactor from / massFactor to))
convertNutrientUnit (NutreintEnergyUnit from) (NutreintEnergyUnit to) value =
  Just (value * (energyFactor from / energyFactor to))
convertNutrientUnit _ _ _ = Nothing

massFactor :: NutrientWeight -> Double
massFactor Gram = 1
massFactor Milligram = 0.001
massFactor Microgram = 0.000001

energyFactor :: NutrientEnergy -> Double
energyFactor Kilocalorie = 1
energyFactor Kilojoule = 1 / 4.184

scaleNutrients :: Double -> [Nutrient] -> Either [Text] [Nutrient]
scaleNutrients grams values
  | grams <= 0 || isNaN grams || isInfinite grams = Left ["Ingredient weight must be finite and positive"]
  | otherwise = do
      validateNutrientAmounts values
      let scaled =
            [ n
                { nutrientAmount = case nutrientUnit n of
                    OtherNutrientUnit _ -> Nothing
                    _ -> (* (grams / 100)) <$> nutrientAmount n
                }
              | n <- values
            ]
      validateNutrientAmounts scaled
      pure scaled

sumNutrition :: [[Nutrient]] -> Either [Text] NutritionReport
sumNutrition contributions = do
  validateNutrientAmounts (concat contributions)
  let perIngredient = [Map.fromList [(nutrientId n, n) | n <- values] | values <- contributions]
      templates = Map.unions perIngredient
      total key template =
        template
          { nutrientAmount = do
              amounts <- traverse (contribution key (nutrientUnit template)) perIngredient
              pure (sum amounts)
          }

      -- Some ingredients can have missing values for certain nutrients.
      -- This does not mean 0, but to keep productive results I default to 0 for these.
      -- I expect the USDA to maintain real values for where the nutrient is important.
      -- A estimated flag is added to warn users of potentially unreliable results.
      contribution key outputUnit values = case Map.lookup key values of
        Nothing -> Just 0
        Just n -> case nutrientUnit n of
          OtherNutrientUnit _ -> Nothing
          _ -> case nutrientAmount n of
            Nothing -> Just 0
            Just amount -> convertNutrientUnit (nutrientUnit n) outputUnit amount
      totals = [total key template | (key, template) <- Map.toAscList templates]
  Control.Monad.when (or (zipWith (\values table -> length values /= Map.size table) contributions perIngredient)) $ Left ["Duplicate nutrient ID within one food response"]
  validateNutrientAmounts totals
  pure (NutritionReport totals)

validateNutrientAmounts :: [Nutrient] -> Either [Text] ()
validateNutrientAmounts values =
  case [ "Invalid or overflowing amount for nutrient " <> T.pack (show (nutrientId n))
         | n <- values,
           Just amount <- [nutrientAmount n],
           amount < 0 || isNaN amount || isInfinite amount
       ] of
    [] -> Right ()
    errors -> Left errors
