{-# LANGUAGE OverloadedStrings #-}

module Api.Helpers
  ( runDb,
    readParam,
    rejectEntry,
    readPantry,
    readNumberParam,
    failWith,
    requireClient,
    nutritionReportResult,
    respond,
  )
where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Aeson
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.Pantry (PantryInput (..))
import Domain.Recipe (Ingredient)
import Integration.Usda
import Network.HTTP.Types
import Text.Read (readMaybe)
import Web.Scotty

runDb :: MVar Connection -> (Connection -> IO a) -> ActionM a
runDb db action = liftIO (withMVar db action)

readParam :: Text -> ActionM Int64
readParam name = do
  value <- pathParam name :: ActionM T.Text
  case readMaybe (T.unpack value) of
    Just rid | rid > 0 -> pure rid
    _ -> do
      status status400
      json (object ["error" .= ("id must be a positive integer" :: T.Text)])
      finish

rejectEntry :: Status -> T.Text -> ActionM a
rejectEntry code message = do
  status code
  json (object ["error" .= message])
  finish

readPantry :: ActionM [Ingredient]
readPantry = do
  payload <- body
  case eitherDecode payload of
    Left errorMessage -> do
      status status400
      json (object ["errors" .= [T.pack errorMessage]])
      finish
    Right (PantryInput items) -> pure items

readNumberParam :: TL.Text -> Int64 -> Int64 -> Int64 -> ActionM Int64
readNumberParam key fallback lower upper = do
  value <- queryParamMaybe key :: ActionM (Maybe T.Text)
  case value of
    Nothing -> pure fallback
    Just rawValue -> case readMaybe (T.unpack rawValue) of
      Just n | n >= lower && n <= upper -> pure n
      _ -> do
        status status400
        json (object ["error" .= (TL.toStrict key <> " must be an integer between " <> T.pack (show lower) <> " and " <> T.pack (show upper))])
        finish

failWith :: Status -> T.Text -> ActionM a
failWith code message = do
  status code
  json (object ["error" .= message])
  finish

requireClient :: Maybe UsdaClient -> ActionM UsdaClient
requireClient Nothing = failWith status503 "Food lookup is not configured; set USDA_API_KEY on the server"
requireClient (Just client) = pure client

respond :: (ToJSON a) => Either UsdaError a -> ActionM ()
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

nutritionReportResult :: ToJSON a => Either NutritionError a -> ActionM ()
nutritionReportResult result = case result of
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
