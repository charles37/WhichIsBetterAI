module Impl.Repository.Comparison.Error (ComparisonRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Data.Text (Text)

-- We want to distinguish the `QueryError` coming from the violation of the "concepts_name_key" unique constraints
data ComparisonRepositoryError
  = DuplicateComparisonName QueryError
  | ComparisonNotFound Text
  | ComparisonNotFoundByName Text
  | EloNotFound Text
  | ConceptNotFound Text
  | ComparisonError Text
  | ModelNotFound Text
  | NoModelsInDatabase
  | DatabaseError QueryError
  | OtherError QueryError
  deriving (Show)
