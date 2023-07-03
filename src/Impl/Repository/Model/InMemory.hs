{-# LANGUAGE OverloadedStrings #-}

module Impl.Repository.Model.InMemory (Table, repository) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Tagger.Model (Model (..))
import Tagger.Id (Id (Id))
import Tagger.Repository.Model (ModelRepository (..))
import Impl.Repository.Model.Error (ModelRepositoryError (..))
import qualified Data.Map as Map 
import Data.Text (Text, pack)

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, assocs, filter, insert, size)
import Data.Text.Encoding (encodeUtf8)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError))
import PostgreSQL.ErrorCodes (unique_violation)
import Prelude hiding (filter)

type Table = TVar (Map (Id Model) Model)

--data Model = Model
--  { --  modelId :: UUID
--    modelName :: Text,
--    modelDescription :: Text,
--    modelName :: Text
--  }
--  deriving stock (Eq, Show, Generic)

-- |
--
--repository :: DB.Handle -> ModelRepository (ExceptT ModelRepositoryError IO)
--repository handle =
--  ModelRepository
--    { selectModel = postgresSelectModel handle,
--      selectAllModels = postgresSelectAllModels handle,
--      getModelByName = postgresGetModelByName handle,
--      addModel = postgresAddModel handle
--    }


--repository :: Table -> UserRepository (ExceptT UserRepositoryError IO)
--repository userMap =
--  UserRepository
--    { findByName = inMemoryGetUserByName userMap,
--      add = inMemoryAddUser userMap
--    }
--
--inMemoryGetUserByName :: Table -> Text -> ExceptT UserRepositoryError IO (Id User, User)
--inMemoryGetUserByName userMap name' = do
--  users <- liftIO $ readTVarIO userMap
--  let usersWithName = filter ((== name') . name) users
--  case size usersWithName of
--    0 -> throwError $ UnexpectedNumberOfRows NoResults
--    1 -> pure . head . assocs $ usersWithName
--    _ -> throwError $ UnexpectedNumberOfRows MoreThanOneResult
--
--duplicateNameError :: Text -> UserRepositoryError
--duplicateNameError name' =
--  DuplicateUserName $
--    QueryError
--      "insert user"
--      []
--      ( ResultError $
--          ServerError
--            unique_violation
--            "duplicate key value violates unique constraint"
--            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
--            Nothing
--      )
--
--inMemoryAddUser :: Table -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
--inMemoryAddUser userMap name' password' = do
--  userId <- Id <$> liftIO nextRandom
--  queryError <- liftIO . atomically $ do
--    users <- readTVar userMap
--    let usersWithName = filter ((== name') . name) users
--    if null usersWithName
--      then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
--      else pure . Just $ duplicateNameError name'
--  case queryError of
--    Just qe -> throwError qe
--    Nothing -> pure userId

repository :: Table -> ModelRepository (ExceptT ModelRepositoryError IO)
repository modelsMap =
  ModelRepository
    { selectModel = inMemorySelectModel modelsMap,
      selectAllModels = inMemorySelectAllModels modelsMap,
      getModelByName = inMemoryGetModelByName modelsMap,
      addModel = inMemoryAddModel modelsMap
    }

inMemorySelectModel :: TVar (Map (Id Model) Model) -> Id Model -> ExceptT ModelRepositoryError IO Model
inMemorySelectModel modelsMap modelId = do
  models <- liftIO $ readTVarIO modelsMap
  case Map.lookup modelId models of
    Nothing -> throwError $ ModelNotFound (pack . show $ modelId)
    Just model -> pure model

inMemoryGetModelByName :: TVar (Map (Id Model) Model) -> Text -> ExceptT ModelRepositoryError IO (Id Model, Model)
inMemoryGetModelByName modelsMap name = do
  models <- liftIO $ readTVarIO modelsMap
  let modelsWithName = filter ((== name) . modelName) models
  case size modelsWithName of
    0 -> throwError $ ModelNotFoundByName ("Name: " <> name) 
    1 -> pure . head . assocs $ modelsWithName 
    _ -> throwError $ duplicateNameError name 


inMemorySelectAllModels :: TVar (Map (Id Model) Model) -> ExceptT ModelRepositoryError IO [(Id Model, Model)]
inMemorySelectAllModels modelsMap = do
    models <- liftIO $ readTVarIO modelsMap
    pure $ assocs models


inMemoryAddModel :: TVar (Map (Id Model) Model) -> Text -> Text -> ExceptT ModelRepositoryError IO (Id Model)
inMemoryAddModel modelsMap mName mDesc = do
  modelId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    models <- readTVar modelsMap
    let modelsWithName = filter ((== mName) . modelName) models
    if null modelsWithName
        then writeTVar modelsMap (insert modelId (Model mName mDesc ) models) >> pure Nothing
        else pure . Just $ duplicateNameError mName
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure modelId

duplicateNameError :: Text -> ModelRepositoryError
duplicateNameError name =
  DuplicateModelName $
    QueryError
      "insert model"
      []
      ( ResultError $
          ServerError
            unique_violation
            "duplicate key value violates unique constraint"
            (Just $ "Key (name)=(" <> encodeUtf8 name <> ") already exists")
            Nothing
      )
--duplicateNameError :: Text -> UserRepositoryError
--duplicateNameError name' =
--  DuplicateUserName $
--    QueryError
--      "insert user"
--      []
--      ( ResultError $
--          ServerError
--            unique_violation
--            "duplicate key value violates unique constraint"
--            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
--            Nothing
--      )
