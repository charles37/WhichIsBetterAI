module Impl.Repository.AIModel.Error (AIModelRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Infrastructure.Persistence.Queries (WrongNumberOfResults)

-- We want to distinguish the `QueryError` coming from the violation of the "ai_models_name_key" unique constraints
data AIModelRepositoryError
  = DuplicateAIModelName QueryError
  | AIModelNotFound
  | DatabaseError QueryError
  deriving (Show)

