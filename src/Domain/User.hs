module Domain.User (CurrentUser, mkCurrentUser, currentUserId) where

import Data.Int (Int64)

newtype CurrentUser = CurrentUser Int64
  deriving (Eq, Show)

mkCurrentUser :: Int64 -> Maybe CurrentUser
mkCurrentUser uid
  | uid > 0 = Just (CurrentUser uid)
  | otherwise = Nothing

currentUserId :: CurrentUser -> Int64
currentUserId (CurrentUser uid) = uid
