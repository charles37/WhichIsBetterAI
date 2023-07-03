{-# LANGUAGE TupleSections #-}

module Impl.Repository.Model.Postgres (repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT, throwE)

import Data.UUID.V4 (nextRandom)
import Data.ByteString (isInfixOf)
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as DB (selectAllModels, selectModelByName, selectModel) --
import Infrastructure.Persistence.Serializer (serializeModel, unserializeModel)
import Tagger.Model (Model (Model))
import Tagger.Id (Id (Id))
import Tagger.Repository.Model (ModelRepository (..))
import Data.Text (Text, pack)
import qualified Infrastructure.Persistence.Queries as Query
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Impl.Repository.Model.Error (ModelRepositoryError (..))
import Infrastructure.Persistence.Schema (litModel, modelId)
--data Model = Model
--  { --  modelId :: UUID
--    modelName :: Text,
--    modelDescription :: Text,
--  }
--  deriving stock (Eq, Show, Generic)


-- |
-- A 'ModelRepository' based on PostgreSQL
repository :: DB.Handle -> ModelRepository (ExceptT ModelRepositoryError IO)
repository handle =
  ModelRepository
    { selectModel = postgresSelectModel handle,
      selectAllModels = postgresSelectAllModels handle,
      getModelByName = postgresGetModelByName handle,
      addModel = postgresAddModel handle
    }

postgresSelectModel :: DB.Handle -> Id Model -> ExceptT ModelRepositoryError IO Model
postgresSelectModel handle modelId' = do
  maybeModel <- runRepositoryQuery handle (DB.selectModel modelId')
  case maybeModel of
    Nothing -> throwE $ ModelNotFound ((pack . show) modelId')    
    Just model -> pure $ unserializeModel model 

postgresSelectAllModels :: DB.Handle -> ExceptT ModelRepositoryError IO [(Id Model, Model)]
postgresSelectAllModels handle = do
    models <- runRepositoryQuery handle DB.selectAllModels
    pure $ map (\model -> (modelId model, unserializeModel model)) models

postgresGetModelByName :: DB.Handle -> Text -> ExceptT ModelRepositoryError IO (Id Model, Model)
postgresGetModelByName handle name = do
    model <- runRepositoryQuery handle (DB.selectModelByName name)
    case model of
      Left _ -> throwE $ ModelNotFound name
      Right model' -> pure $ (modelId model', unserializeModel model')

postgresAddModel :: DB.Handle -> Text -> Text -> ExceptT ModelRepositoryError IO (Id Model)
postgresAddModel handle mName mDesc = do
  -- Generate a UUID for the model
  modelUUID <- liftIO nextRandom

  let query = Query.addModel . litModel $ serializeModel (Id modelUUID) (Model mName mDesc)

  runRepositoryQuery handle query
  pure $ Id modelUUID


-- | Run a query transforming a Hasql.QueryError into a ModelRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT ModelRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> ModelRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "models_model_name_key" `isInfixOf` message = DuplicateModelName queryError
liftRepositoryError queryError = OtherError queryError

-- |
-- A 'UserRepository' based on PostgreSQL
--repository :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
--repository handle =
--  UserRepository
--    { findByName = postgresGetUserByName handle,
--      add = postgresAddUser handle
--    }
--
--postgresGetUserByName :: DB.Handle -> Text -> ExceptT UserRepositoryError IO (Id User, User)
--postgresGetUserByName handle name = do
--  eitherUser <- runRepositoryQuery handle (Query.selectUserByName name)
--  case eitherUser of
--    Right usr -> pure (userId usr, unserializeUser usr)
--    Left e -> throwE $ UnexpectedNumberOfRows e

--postgresAddUser :: DB.Handle -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
--postgresAddUser handle name password = do
--  -- Generate the UUID for the user
--  userId' <- liftIO nextRandom
--  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)
--
--  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
--  runRepositoryQuery handle query
--  pure $ Id userId'
--
--


-- | Run a query transforming a Hasql.QueryError into a UserRepositoryError as appropriate to the
-- domain.
--runRepositoryQuery :: DB.Handle -> Session a -> ExceptT UserRepositoryError IO a
--runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle
--
--liftRepositoryError :: QueryError -> UserRepositoryError
--liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
--  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
--liftRepositoryError queryError = OtherError queryError
