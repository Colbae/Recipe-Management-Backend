{-# LANGUAGE OverloadedStrings #-}

module Domain.RecipeParser (recipeParser) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Domain.IngredientParser (ingredientParser)
import Domain.Recipe (Ingredient, RecipeInput, mkRecipeInput)
import Text.Megaparsec (MonadParsec (eof, lookAhead), Parsec, manyTill, some, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data SectionHeader
  = TitleHeader
  | ServingsHeader
  | IngredientsHeader
  | InstructionsHeader
  deriving (Show, Eq)

data RecipeSection
  = TitleSection Text
  | ServingsSection Int
  | IngredientsSection [Ingredient]
  | InstructionsSection [Text]

data PartialRecipe = PartialRecipe
  { prTitle :: Maybe Text,
    prServings :: Maybe Int,
    prIngredients :: Maybe [Ingredient],
    prInstructions :: Maybe [Text]
  }

sectionHeaderParser :: Parser SectionHeader
sectionHeaderParser = do
  header <-
    T.toLower . T.strip
      <$> takeWhile1P
        (Just "section header")
        (\c -> c /= ':' && c /= '\n' && c /= '\r')
  _ <- char ':'
  case header of
    "title" -> pure TitleHeader
    "servings" -> pure ServingsHeader
    "ingredients" -> pure IngredientsHeader
    "instructions" -> pure InstructionsHeader
    _ -> fail $ "Unknown section: " <> T.unpack header

instructionsParser :: Parser Text
instructionsParser =
  T.strip
    <$> takeWhile1P
      (Just "instruction")
      (\c -> c /= '\n' && c /= '\r') -- parse until new line

titleParser :: Parser Text
titleParser =
  T.strip
    <$> takeWhile1P
      (Just "recipe title")
      (\c -> c /= '\n' && c /= '\r') -- parse until new line

servingsParser :: Parser Int
servingsParser = do
  value <- L.decimal :: Parser Integer
  if value > 0 && value <= toInteger (maxBound :: Int)
    then pure (fromInteger value)
    else fail "Servings must be a positive Int"

ingredientListParser :: Parser [Ingredient]
ingredientListParser =
  manyTill
    (ingredientParser <* lineEnd <* space)
    sectionEnd

instructionListParser :: Parser [Text]
instructionListParser =
  manyTill
    (instructionsParser <* lineEnd <* space)
    sectionEnd

sectionEnd :: Parser ()
sectionEnd = lookAhead (try (void sectionHeaderParser) <|> eof)

lineEnd :: Parser ()
lineEnd = hspace *> (void eol <|> eof)

sectionParser :: Parser RecipeSection
sectionParser = do
  header <- sectionHeaderParser
  hspace
  case header of
    TitleHeader ->
      TitleSection <$> (titleParser <* lineEnd)
    ServingsHeader ->
      ServingsSection <$> (servingsParser <* lineEnd)
    IngredientsHeader ->
      IngredientsSection <$> (lineEnd *> space *> ingredientListParser)
    InstructionsHeader ->
      InstructionsSection <$> (lineEnd *> space *> instructionListParser)

emptyRecipe :: PartialRecipe
emptyRecipe = PartialRecipe Nothing Nothing Nothing Nothing

addSection :: PartialRecipe -> RecipeSection -> Parser PartialRecipe
addSection r section =
  case section of
    TitleSection x ->
      setOnce "title" (prTitle r) $
        r {prTitle = Just x}
    ServingsSection x ->
      setOnce "servings" (prServings r) $
        r {prServings = Just x}
    IngredientsSection x ->
      setOnce "ingredients" (prIngredients r) $
        r {prIngredients = Just x}
    InstructionsSection x ->
      setOnce "instructions" (prInstructions r) $
        r {prInstructions = Just x}
  where
    setOnce name existing updated =
      case existing of
        Just _ -> fail ("Duplicate section: " <> name)
        Nothing -> pure updated

buildRecipe :: [RecipeSection] -> Parser RecipeInput
buildRecipe sections = do
  r <- foldM addSection emptyRecipe sections

  title <- required "title" (prTitle r)
  servings <- required "servings" (prServings r)
  ingredients <- required "ingredients" (prIngredients r)
  instructions <- required "instructions" (prInstructions r)

  case mkRecipeInput title servings ingredients instructions of
    Left errors -> fail (T.unpack (T.intercalate "; " errors))
    Right recipe -> pure recipe
  where
    required name =
      maybe (fail ("Missing section: " <> name)) pure

recipeParser :: Parser RecipeInput
recipeParser = do
  space
  sections <- some (sectionParser <* space)
  eof
  buildRecipe sections
