{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Food (Provider (..), FoodReference (..), FoodSummary (..), FoodSearch (..), FoodDetails (..)) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Nutrition (Nutrient)

data Provider = USDA -- Only USDA supported at the moment
  deriving (Eq, Show)

instance FromJSON Provider where
  parseJSON = withText "Provider" $ \case
    "usda" -> pure USDA
    _ -> fail "Unsupported provider"

instance ToJSON Provider where
  toJSON USDA = String "usda"

data FoodReference = FoodReference
  { provider :: Provider,
    foodId :: Int64
  }
  deriving (Eq, Show)

instance FromJSON FoodReference where
  parseJSON = withObject "FoodReference" $ \o -> FoodReference <$> o .: "provider" <*> o .: "foodId"

instance ToJSON FoodReference where
  toJSON r = object ["provider" .= provider r, "foodId" .= foodId r]

data FoodSummary = FoodSummary
  { summaryId :: Int64,
    summaryDescription :: Text,
    summaryDataType :: Text
  }
  deriving (Eq, Show)

instance ToJSON FoodSummary where
  toJSON f =
    object
      [ "provider" .= USDA,
        "foodId" .= summaryId f,
        "description" .= summaryDescription f,
        "dataType" .= summaryDataType f
      ]

data FoodSearch = FoodSearch
  { searchTotalHits :: Int,
    searchPage :: Int,
    searchTotalPages :: Int,
    searchResults :: [FoodSummary]
  }
  deriving (Eq, Show)

instance ToJSON FoodSearch where
  toJSON s =
    object
      [ "totalHits" .= searchTotalHits s,
        "page" .= searchPage s,
        "totalPages" .= searchTotalPages s,
        "foods" .= searchResults s
      ]

data FoodDetails = FoodDetails
  { detailsSummary :: FoodSummary,
    detailsNutrients :: [Nutrient]
  }
  deriving (Eq, Show)

instance ToJSON FoodDetails where
  toJSON f =
    object
      [ "provider" .= USDA,
        "foodId" .= summaryId (detailsSummary f),
        "description" .= summaryDescription (detailsSummary f),
        "dataType" .= summaryDataType (detailsSummary f),
        "nutrients" .= detailsNutrients f
      ]
