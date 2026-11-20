{-# LANGUAGE OverloadedStrings #-}

module Domain.User (CurrentUser, mkCurrentUser, currentUserId, User (..)) where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Data.Int (Int64)
import Data.Text

newtype CurrentUser = CurrentUser Int64
  deriving (Eq, Show)

mkCurrentUser :: Int64 -> Maybe CurrentUser
mkCurrentUser uid
  | uid > 0 = Just (CurrentUser uid)
  | otherwise = Nothing

currentUserId :: CurrentUser -> Int64
currentUserId (CurrentUser uid) = uid

data User = User
  { id :: Int64,
    name :: Text
  }
  deriving (Eq, Show)

instance ToJSON User where
  toJSON plan =
    object
      [ "id" .= Domain.User.id plan,
        "name" .= name plan
      ]
