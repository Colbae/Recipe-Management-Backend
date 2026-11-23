{-# LANGUAGE OverloadedStrings #-}

module Domain.Units where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Text

data Volume = Liter | DeciLiter | Mililiter | Teaspoon | Tablespoon
  deriving (Show, Eq)

data Weight = Kilogram | Gram
  deriving (Show, Eq)

data Count = Count
  deriving (Show, Eq)

data Unit = VolumeUnit Volume | WeightUnit Weight | CountUnit Count
  deriving (Show, Eq)

unitText :: Unit -> Text
unitText (VolumeUnit Liter) = "l"
unitText (VolumeUnit DeciLiter) = "dl"
unitText (VolumeUnit Mililiter) = "ml"
unitText (VolumeUnit Teaspoon) = "tsp"
unitText (VolumeUnit Tablespoon) = "tbsp"
unitText (WeightUnit Kilogram) = "kg"
unitText (WeightUnit Gram) = "g"
unitText (CountUnit Count) = "piece"

instance ToJSON Unit where
  toJSON = String . unitText

parseUnit :: Text -> Maybe Unit
parseUnit "l" = Just (VolumeUnit Liter)
parseUnit "dl" = Just (VolumeUnit DeciLiter)
parseUnit "ml" = Just (VolumeUnit Mililiter)
parseUnit "tsp" = Just (VolumeUnit Teaspoon)
parseUnit "tbsp" = Just (VolumeUnit Tablespoon)
parseUnit "kg" = Just (WeightUnit Kilogram)
parseUnit "g" = Just (WeightUnit Gram)
parseUnit "piece" = Just (CountUnit Count)
parseUnit _ = Nothing

instance FromJSON Unit where
  parseJSON = withText "Unit" $ \text ->
    case parseUnit text of
      Just u -> pure u
      Nothing ->
        fail "Unsupported unit! expected l, dl, ml, tsp, tbsp, kg, g or piece"

volumeInLiters :: Volume -> Double
volumeInLiters Liter = 1
volumeInLiters DeciLiter = 0.1
volumeInLiters Mililiter = 0.001
volumeInLiters Teaspoon = 0.005
volumeInLiters Tablespoon = 0.015

convertVolume :: Volume -> Volume -> Double -> Double
convertVolume from to amount = amount * (volumeInLiters from / volumeInLiters to)

weightInKilograms :: Weight -> Double
weightInKilograms Gram = 0.001
weightInKilograms Kilogram = 1

convertWeigth :: Weight -> Weight -> Double -> Double
convertWeigth from to amount = amount * (weightInKilograms from / weightInKilograms to)

convertUnits :: Unit -> Unit -> Double -> Maybe Double
convertUnits (VolumeUnit from) (VolumeUnit to) amount = Just (convertVolume from to amount)
convertUnits (WeightUnit from) (WeightUnit to) amount = Just (convertWeigth from to amount)
convertUnits (CountUnit Count) (CountUnit Count) amount = Just amount
convertUnits _ _ _ = Nothing
