module Impl.Repository.Concept.Error (ConceptRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Data.Text (Text)

-- We want to distinguish the `QueryError` coming from the violation of the "concepts_name_key" unique constraints
data ConceptRepositoryError
  = DuplicateConceptName QueryError
  | DuplicateConceptWikiLink QueryError
  | ConceptNotFound Text
  | ConceptNotFoundByWikiLink Text
  | DatabaseError QueryError
  | OtherError QueryError
  deriving (Show)

