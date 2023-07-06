module Impl.Repository.Concept.Error (ConceptRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Data.Text (Text)

data ConceptRepositoryError
  = DuplicateConceptName QueryError
  | DuplicateConceptWikiLink QueryError
  | ConceptNotFound Text
  | ConceptNotFoundByWikiLink Text
  | DatabaseError QueryError
  | OtherError QueryError
  deriving (Show)

