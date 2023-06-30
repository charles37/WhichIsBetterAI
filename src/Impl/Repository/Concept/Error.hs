module Impl.Repository.Concept.Error (ConceptRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Infrastructure.Persistence.Queries (WrongNumberOfResults)

-- We want to distinguish the `QueryError` coming from the violation of the "concepts_name_key" unique constraints
data ConceptRepositoryError
  = DuplicateConceptName QueryError
  | ConceptNotFound
  | DatabaseError QueryError
  deriving (Show)

