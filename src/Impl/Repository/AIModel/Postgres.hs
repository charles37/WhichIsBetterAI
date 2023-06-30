{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Impl.Repository.AIModel.Postgres (repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, withExceptT)
import Data.ByteString (isInfixOf)
import Data.Text (Text)
import Data.UUID.V4 (nextRandom)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Impl.Repository.AIModel.Error (AIModelRepositoryError (..))
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as Query
import Infrastructure.Persistence.Schema (litAIModel, modelId)
import Infrastructure.Persistence.Serializer (serializeAIModel, unserializeAIModel)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id (Id))
import Tagger.Repository.AIModel (AIModelRepository (..))
import Tagger.AIModel (AIModel (AIModel))

-- |
-- An 'AIModelRepository' based on PostgreSQL
repository :: DB.Handle -> AIModelRepository (ExceptT AIModelRepositoryError IO)
repository handle =
  AIModelRepository
    { findByName = postgresGetAIModelByName handle,
      add = postgresAddAIModel handle
    }

postgresGetAIModelByName :: DB.Handle -> Text -> ExceptT AIModelRepositoryError IO (Id AIModel, AIModel)
postgresGetAIModelByName handle name = do
  eitherAIModel <- runRepositoryQuery handle (Query.selectAIModelByName name)
  case eitherAIModel of
    Right usr -> pure (modelId usr, unserializeAIModel usr)
    Left e -> throwE $ UnexpectedNumberOfRows e

postgresAddAIModel :: DB.Handle -> Text -> ExceptT AIModelRepositoryError IO (Id AIModel)
postgresAddAIModel handle name = do
  -- Generate the UUID for the AIModel
  modelId' <- liftIO nextRandom
  let query = Query.addAIModel . litAIModel $ serializeAIModel (Id modelId') (AIModel name)

  -- Actually add the AIModel to the database, differentiating the `AIModelRepositoryError` cases
  runRepositoryQuery handle query
  pure $ Id modelId'

-- | Run a query transforming a Hasql.QueryError into a AIModelRepositoryError as appropriate to the
-- domain.
runRepositoryQuery :: DB.Handle -> Session a -> ExceptT AIModelRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> AIModelRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "ai_models_name_key" `isInfixOf` message = DuplicateAIModelName queryError
liftRepositoryError queryError = OtherError queryError

