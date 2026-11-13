{-# LANGUAGE OverloadedStrings #-}

module Api.Pantry (pantryRoutes) where

import Api.Profiles (ProfileSelection, requireCurrentProfile)
import Api.Helpers (readId, rejectEntry, runDb, readPantry)
import Control.Concurrent.MVar (MVar)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (Connection)
import Domain.Pantry (PantryError (UserNotFound), pantryId)
import Network.HTTP.Types
import qualified Storage.Pantry as Store
import Web.Scotty

pantryRoutes :: MVar Connection -> ProfileSelection -> ScottyM ()
pantryRoutes db profiles = do
  get "/pantry/:id" $ do
    current <- requireCurrentProfile db profiles
    pId <- readId
    runDb db (\connection -> Store.getPantry connection current pId)
      >>= maybe (rejectEntry status404 "Pantry not found") json
  post "/pantry" $ do
    current <- requireCurrentProfile db profiles
    p <- readPantry
    results <- runDb db (\connection -> Store.createPantry connection current p)
    case results of
      Left err -> rejectPantryError err
      Right pantry -> do
        status status201
        setHeader "Location" (TL.pack ("/pantry/" ++ show (pantryId pantry)))
        json pantry
  post "/pantry/:id/add" $ do
    current <- requireCurrentProfile db profiles
    pId <- readId
    p <- readPantry
    results <- runDb db (\connection -> Store.addIngredients connection current pId p)
    case results of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> json pantry
  post "/pantry/:id/remove" $ do
    current <- requireCurrentProfile db profiles
    pId <- readId
    p <- readPantry
    results <- runDb db (\connection -> Store.removeIngredients connection current pId p)
    case results of
      Nothing -> rejectEntry status404 "Pantry not found"
      Just pantry -> json pantry

rejectPantryError :: PantryError -> ActionM a
rejectPantryError UserNotFound = rejectEntry status404 "User not found"
