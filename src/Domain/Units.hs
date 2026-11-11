{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use lambda-case" -}

module Domain.Units where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)

data Volume = Liter | DeciLiter | Mililiter | Teaspoon | Tablespoon
  deriving (Show, Eq)

data Weight = Kilogram | Gram
  deriving (Show, Eq)

data Count = Count
  deriving (Show, Eq)

data Unit = VolumeUnit Volume | WeightUnit Weight | CountUnit Count
  deriving (Show, Eq)

instance ToJSON Unit where
  toJSON (VolumeUnit Liter) = String "l"
  toJSON (VolumeUnit DeciLiter) = String "dl"
  toJSON (VolumeUnit Mililiter) = String "ml"
  toJSON (VolumeUnit Teaspoon) = String "tsp"
  toJSON (VolumeUnit Tablespoon) = String "tbsp"
  toJSON (WeightUnit Kilogram) = String "kg"
  toJSON (WeightUnit Gram) = String "g"
  toJSON (CountUnit Count) = String "piece"

instance FromJSON Unit where
  parseJSON = withText "Unit" $ \value -> case value of
    "l" -> pure (VolumeUnit Liter)
    "dl" -> pure (VolumeUnit DeciLiter)
    "ml" -> pure (VolumeUnit Mililiter)
    "tsp" -> pure (VolumeUnit Teaspoon)
    "tbsp" -> pure (VolumeUnit Tablespoon)
    "kg" -> pure (WeightUnit Kilogram)
    "g" -> pure (WeightUnit Gram)
    "piece" -> pure (CountUnit Count)
    _ -> fail "Unsupported unit! expected l, dl, ml, tsp, tbsp, kg, g or piece"

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
