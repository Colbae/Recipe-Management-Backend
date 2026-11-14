{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Domain.IngredientParser (ingredientParser) where

import Data.Char (isLetter)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Domain.Food (FoodReference (FoodReference), Provider (USDA))
import Domain.Recipe (Ingredient, mkIngredients)
import Domain.Units (Unit, parseUnit)
import Text.Megaparsec (Parsec, optional, takeWhile1P)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

providerParser :: Parser Provider
providerParser = do
  provider <- takeWhile1P (Just "food reference provider") isLetter
  case provider of
    "usda" -> pure USDA
    _ -> fail ("Unknown provider: " ++ T.unpack provider)

foodIdParser :: Parser Int64
foodIdParser = do
  value <- L.decimal :: Parser Integer
  if value > 0 && value <= toInteger (maxBound :: Int64)
    then pure (fromInteger value)
    else fail "Food ID must be a positive Int64"

foodReferenceParser :: Parser FoodReference
foodReferenceParser = do
  provider <- providerParser
  _ <- char ':'
  FoodReference provider <$> foodIdParser

amountParser :: Parser Double
amountParser = realToFrac <$> L.scientific

unitParser :: Parser Unit
unitParser = do
  symbol <- takeWhile1P (Just "ingredient unit") isLetter
  case parseUnit symbol of
    Just unit -> pure unit
    Nothing -> fail ("Unknown unit: " ++ T.unpack symbol)

titleParser :: Parser Text
titleParser =
  T.strip
    <$> takeWhile1P
      (Just "ingredient name")
      (\c -> c /= '@' && c /= '\n' && c /= '\r') -- consume everything until the end or optional @

ingredientParser :: Parser Ingredient
ingredientParser = do
  amount <- amountParser
  hspace1
  unit <- unitParser
  hspace1
  name <- titleParser
  reference <- optional (char '@' *> foodReferenceParser)
  either (fail . show) pure (mkIngredients name amount unit reference)