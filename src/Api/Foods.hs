{-# LANGUAGE OverloadedStrings #-}
module Api.Foods (foodRoutes) where

import Data.Aeson (ToJSON, object, (.=))
import Api.Helpers (readPantry)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Integration.Usda
import Network.HTTP.Types.Status
import Text.Read (readMaybe)
import Web.Scotty

foodRoutes :: Maybe UsdaClient -> ScottyM ()
foodRoutes configured = do
  post "/foods/nutrition" $ do
    ingredients <- readPantry
    client <- requireClient configured
    result <- liftIO (generateNutritionReport client ingredients)
    case result of
      Right report -> json report
      Left err -> case err of
        MissingFoodReference label -> failWith status400 ("Missing food reference: " <> label)
        UnsupportedQuantity label -> failWith status400 ("Nutrition requires a weight in grams or kilograms: " <> label)
        NutritionLookupFailed _ upstream -> respond (Left upstream :: Either UsdaError ())
        UnsupportedNutritionDataType label kind -> failWith status422 ("Unsupported food data type for " <> label <> ": " <> kind)
        NoNutrientData label -> failWith status422 ("No nutrient data for " <> label)
        NutritionCalculationFailed errors -> do
          status status422
          json (object ["errors" .= errors])
  get "/foods/search" $ do
    query <- queryParamMaybe "query" :: ActionM (Maybe Text)
    term <- case T.strip <$> query of
      Just q | not (T.null q) && T.length q <= 200 -> pure q
      _ -> failWith status400 "query must contain 1 to 200 characters"
    page <- integerParam "page" 1 1 10000
    pageSize <- integerParam "pageSize" 20 1 200
    client <- requireClient configured
    liftIO (searchFoods client term page pageSize) >>= respond
  get "/foods/:id" $ do
    rawId <- pathParam "id" :: ActionM Text
    fid <- case readMaybe (T.unpack rawId) :: Maybe Int64 of
      Just n | n > 0 -> pure n
      _ -> failWith status400 "food id must be a positive integer"
    client <- requireClient configured
    liftIO (getFood client fid) >>= respond

integerParam :: Text -> Int -> Int -> Int -> ActionM Int
integerParam key fallback lower upper = do
  value <- queryParamMaybe (TL.fromStrict key) :: ActionM (Maybe Text)
  case value of
    Nothing -> pure fallback
    Just rawValue -> case readMaybe (T.unpack rawValue) of
      Just n | n >= lower && n <= upper -> pure n
      _ -> failWith status400 (key <> " must be an integer between " <> T.pack (show lower) <> " and " <> T.pack (show upper))

requireClient :: Maybe UsdaClient -> ActionM UsdaClient
requireClient Nothing = failWith status503 "Food lookup is not configured; set USDA_API_KEY on the server"
requireClient (Just client) = pure client

respond :: ToJSON a => Either UsdaError a -> ActionM ()
respond (Right result) = json result
respond (Left err) = case err of
  FoodNotFound -> failWith status404 "USDA food not found"
  RateLimited -> do
    setHeader "Retry-After" "3600"
    failWith status503 "USDA rate limit reached; try again later"
  UpstreamTimeout -> failWith status504 "USDA request timed out"
  UpstreamCredentialsRejected -> failWith status502 "USDA rejected the server API credentials"
  InvalidUpstreamResponse -> failWith status502 "USDA returned an unexpected response"
  UpstreamUnavailable -> failWith status502 "USDA is unavailable"

failWith :: Status -> Text -> ActionM a
failWith code message = do
  status code
  json (object ["error" .= message])
  finish
