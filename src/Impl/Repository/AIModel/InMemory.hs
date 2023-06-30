module Impl.Repository.AIModel.InMemory (Table, repository) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, assocs, filter, insert, size)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4 (nextRandom)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError))
import Impl.Repository.AIModel.Error (AIModelRepositoryError (..))
import Infrastructure.Persistence.Queries (WrongNumberOfResults (..))
import PostgreSQL.ErrorCodes (unique_violation)
import Tagger.Id (Id (Id))
import Tagger.Repository.AIModel (AIModelRepository (..))
import Tagger.AIModel (AIModel (..))
import Prelude hiding (filter)

type Table = TVar (Map (Id AIModel) AIModel)

repository :: Table -> AIModelRepository (ExceptT AIModelRepositoryError IO)
repository aiModelMap =
  AIModelRepository
    { findByName = inMemoryGetAIModelByName aiModelMap,
      add = inMemoryAddAIModel aiModelMap
    }

inMemoryGetAIModelByName :: Table -> Text -> ExceptT AIModelRepositoryError IO (Id AIModel, AIModel)
inMemoryGetAIModelByName aiModelMap name' = do
  aiModels <- liftIO $ readTVarIO aiModelMap
  let aiModelsWithName = filter ((== name') . modelName) aiModels
  case size aiModelsWithName of
    0 -> throwError AIModelNotFound
    1 -> pure . head . assocs $ aiModelsWithName
    _ -> throwError $ UnexpectedNumberOfRows MoreThanOneResult

duplicateNameError :: Text -> AIModelRepositoryError
duplicateNameError name' =
  DuplicateAIModelName $
    QueryError
      "insert AIModel"
      []
      ( ResultError $
          ServerError
            unique_violation
            "duplicate key value violates unique constraint"
            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
            Nothing
      )

inMemoryAddAIModel :: Table -> Text -> ExceptT AIModelRepositoryError IO (Id AIModel)
inMemoryAddAIModel aiModelMap name' = do
  modelId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    aiModels <- readTVar aiModelMap
    let aiModelsWithName = filter ((== name') . modelName) aiModels
    if null aiModelsWithName
      then writeTVar aiModelMap (insert modelId (AIModel name') aiModels) >> pure Nothing
      else pure . Just $ duplicateNameError name'
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure modelId

