
module Impl.Repository.Elo.Error (EloRepositoryError (..)) where

import Hasql.Session (QueryError (..))
import Data.Text (Text)

-- We want to distinguish the `QueryError` coming from the violation of the "concepts_name_key" unique constraints
data EloRepositoryError
  = DuplicateEloName QueryError
  | EloNotFound Text
  | EloNotFoundByName Text
  | DatabaseError QueryError
  | OtherError QueryError
  deriving (Show)

