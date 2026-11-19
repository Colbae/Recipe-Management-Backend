{-# LANGUAGE OverloadedStrings #-}

module Domain.Validation
  ( Validation,
    validationResult,
    fromEither,
    invalid,
    prefixErrors,
    andThen,
    checkedField,
    field,
    checkedJSON,
  )
where

import Data.Aeson (FromJSON, Object, (.:))
import qualified Data.Aeson as Key
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T

newtype Validation a = Validation {validationResult :: Either [Text] a}

instance Functor Validation where
  fmap f (Validation result) = Validation (fmap f result)

instance Applicative Validation where
  pure = Validation . Right
  Validation (Left a) <*> Validation (Left b) = Validation (Left (a ++ b))
  Validation (Left errors) <*> _ = Validation (Left errors)
  _ <*> Validation (Left errors) = Validation (Left errors)
  Validation (Right f) <*> Validation (Right value) = pure (f value)

fromEither :: Either [Text] a -> Validation a
fromEither = Validation

invalid :: Text -> Validation a
invalid = Validation . Left . pure

prefixErrors :: Text -> Validation a -> Validation a
prefixErrors label (Validation result) = Validation $ case result of
  Left errors -> Left (map (\message -> label <> T.pack ": " <> message) errors)
  Right value -> Right value

andThen :: Validation a -> (a -> Validation b) -> Validation b
andThen (Validation (Left errors)) _ = Validation (Left errors)
andThen (Validation (Right value)) next = next value

field :: (FromJSON a) => Object -> Key.Key -> Validation a
field o key =
  prefixErrors (Key.toText key) $
    fromEither (either (Left . pure . T.pack) Right (parseEither (.: key) o))

checkedField :: (FromJSON a) => Object -> Key.Key -> (a -> [Text]) -> Validation a
checkedField o key check =
  field o key `andThen` \value ->
    case check value of
      [] -> pure value
      errors -> fromEither (Left (map (\message -> Key.toText key <> ": " <> message) errors))

checkedJSON :: Either [Text] a -> Parser a
checkedJSON = either (fail . T.unpack . T.intercalate "; ") pure