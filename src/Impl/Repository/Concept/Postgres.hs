{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}

module Impl.Repository.Concept.Postgres (repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE, withExceptT)
import Hasql.Session (Session)
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as Query
import Tagger.Concept (Concept(..))
import Tagger.Repository.Concept (ConceptRepository(..), ConceptRepositoryError(..))

repository :: DB.Handle -> ConceptRepository (ExceptT ConceptRepositoryError IO)
repository handle =
  ConceptRepository
    { findByConceptName = postgresGetConceptByName handle,
      addConcept = postgresAddConcept handle
    }

postgresGetConceptByName :: DB.Handle -> Text -> ExceptT ConceptRepositoryError IO Concept
postgresGetConceptByName handle name = do
  eitherConcept <- runRepositoryQuery handle (Query.selectConceptByName name)
  case eitherConcept of
    Right concept -> pure concept
    Left _ -> throwE ConceptNotFound

postgresAddConcept :: DB.Handle -> Concept -> ExceptT ConceptRepositoryError IO ()
postgresAddConcept handle concept = do
  _ <- runRepositoryQuery handle (Query.addConcept concept)
  pure ()

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT ConceptRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> ConceptRepositoryError
liftRepositoryError _ = DatabaseError

