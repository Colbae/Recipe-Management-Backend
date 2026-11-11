{-# LANGUAGE OverloadedStrings #-}

module Domain.Food (FoodReference (..), FoodSummary (..), FoodSearch (..), FoodDetails (..), Nutrient (..)) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)

data FoodReference = FoodReference
  { provider :: Text,
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
      [ "provider" .= ("usda" :: Text),
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

-- TODO: Add Unit type here too!
data Nutrient = Nutrient
  { nutrientId :: Int,
    nutrientName :: Text,
    nutrientUnit :: Text,
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

data FoodDetails = FoodDetails
  { detailsSummary :: FoodSummary,
    detailsNutrients :: [Nutrient]
  }
  deriving (Eq, Show)

instance ToJSON FoodDetails where
  toJSON f =
    object
      [ "provider" .= ("usda" :: Text),
        "foodId" .= summaryId (detailsSummary f),
        "description" .= summaryDescription (detailsSummary f),
        "dataType" .= summaryDataType (detailsSummary f),
        "nutrients" .= detailsNutrients f
      ]
