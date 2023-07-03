
module Impl.Repository.Model.Error (ModelRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Data.Text (Text)

-- We want to distinguish the `QueryError` coming from the violation of the "concepts_name_key" unique constraints
data ModelRepositoryError
  = DuplicateModelName QueryError
  | ModelNotFound Text
  | ModelNotFoundByName Text
  | DatabaseError QueryError
  | OtherError QueryError
  deriving (Show)
