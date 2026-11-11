{-# LANGUAGE OverloadedStrings #-}

module Integration.Usda (UsdaClient, UsdaError (..), newClient, searchFoods, getFood) where

import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Domain.Food
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

data UsdaClient = UsdaClient Manager Text String

data UsdaError
  = FoodNotFound
  | RateLimited
  | UpstreamUnavailable
  | UpstreamTimeout
  | UpstreamCredentialsRejected
  | InvalidUpstreamResponse
  deriving (Eq, Show)

newClient :: Manager -> Text -> String -> UsdaClient
newClient = UsdaClient

searchFoods :: UsdaClient -> Text -> Int -> Int -> IO (Either UsdaError FoodSearch)
searchFoods client query page pageSize = do
  result <-
    send
      client
      "/foods/search"
      ( Just
          ( object
              [ "query" .= query,
                "pageNumber" .= page,
                "pageSize" .= pageSize,
                "dataType" .= (["Foundation", "SR Legacy"] :: [Text])
              ]
          )
      )
  pure (result >>= parseResponse parseSearch)

getFood :: UsdaClient -> Int64 -> IO (Either UsdaError FoodDetails)
getFood client fid = do
  result <- send client ("/food/" ++ show fid) Nothing
  pure (result >>= parseResponse parseDetails)

send :: UsdaClient -> String -> Maybe Value -> IO (Either UsdaError Value)
send (UsdaClient manager key base) endpoint payload = do
  response <- try $ do
    initial <- parseRequest (base ++ endpoint)
    let request =
          setQueryString
            [("api_key", Just (TE.encodeUtf8 key))]
            initial
              { method = maybe "GET" (const "POST") payload,
                requestBody = maybe (RequestBodyBS BS.empty) (RequestBodyLBS . encode) payload,
                requestHeaders = [("Accept", "application/json"), ("Content-Type", "application/json")],
                responseTimeout = responseTimeoutMicro (10 * 1000000),
                redirectCount = 0,
                checkResponse = \_ _ -> pure ()
              }
    httpLbs request manager
  pure $ case response of
    Left (HttpExceptionRequest _ ResponseTimeout) -> Left UpstreamTimeout
    Left (HttpExceptionRequest _ ConnectionTimeout) -> Left UpstreamTimeout
    Left (InvalidUrlException _ _) -> Left UpstreamUnavailable
    Left (HttpExceptionRequest _ _) -> Left UpstreamUnavailable
    Right r -> case statusCode (responseStatus r) of
      200 -> either (const (Left InvalidUpstreamResponse)) Right (eitherDecode (responseBody r))
      404 -> Left FoodNotFound
      429 -> Left RateLimited
      401 -> Left UpstreamCredentialsRejected
      403 -> Left UpstreamCredentialsRejected
      _ -> Left UpstreamUnavailable

parseResponse :: (Value -> Parser a) -> Value -> Either UsdaError a
parseResponse parser = either (const (Left InvalidUpstreamResponse)) Right . parseEither parser

parseSummary :: Value -> Parser FoodSummary
parseSummary = withObject "USDA food" $ \o ->
  FoodSummary <$> o .: "fdcId" <*> o .: "description" <*> o .: "dataType"

parseSearch :: Value -> Parser FoodSearch
parseSearch = withObject "USDA search" $ \o -> do
  foods <- o .: "foods" >>= mapM parseSummary
  FoodSearch <$> o .: "totalHits" <*> o .: "currentPage" <*> o .: "totalPages" <*> pure foods

parseDetails :: Value -> Parser FoodDetails
parseDetails value =
  withObject
    "USDA food details"
    ( \o -> do
        summary <- parseSummary value
        nutrients <- o .: "foodNutrients" >>= mapM parseNutrient
        pure (FoodDetails summary nutrients)
    )
    value

parseNutrient :: Value -> Parser Nutrient
parseNutrient = withObject "USDA nutrient value" $ \o -> do
  nutrient <- o .: "nutrient"
  Nutrient <$> nutrient .: "id" <*> nutrient .: "name" <*> nutrient .: "unitName" <*> o .:? "amount"
