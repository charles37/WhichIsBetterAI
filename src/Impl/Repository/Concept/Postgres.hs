{-# LANGUAGE TupleSections #-}

module Impl.Repository.Concept.Postgres (repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT, throwE)

import Data.UUID.V4 (nextRandom)
import Data.ByteString (isInfixOf)
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as DB (selectAllConcepts, selectConceptByWikiLink, selectConcept) --
import Infrastructure.Persistence.Serializer (serializeConcept, unserializeConcept, serializeElo)
import Tagger.Concept (Concept (Concept))
import Tagger.Id (Id (Id))
import Tagger.Repository.Concept (ConceptRepository (..))
import Data.Text (Text, pack)
import qualified Infrastructure.Persistence.Queries as Query
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Impl.Repository.Concept.Error (ConceptRepositoryError (..))
import Infrastructure.Persistence.Schema (litConcept, conceptId, modelId, litElo)

import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (Day(..))
import Tagger.Elo (Elo (..))

--data Concept = Concept
--  { --  conceptId :: UUID
--    conceptName :: Text,
--    conceptDescription :: Text,
--    conceptWikiLink :: Text
--  }
--  deriving stock (Eq, Show, Generic)


-- |
-- A 'ConceptRepository' based on PostgreSQL
repository :: DB.Handle -> ConceptRepository (ExceptT ConceptRepositoryError IO)
repository handle =
  ConceptRepository
    { selectConcept = postgresSelectConcept handle,
      selectAllConcepts = postgresSelectAllConcepts handle,
      getConceptByWikiLink = postgresGetConceptByWikiLink handle,
      addConcept = postgresAddConcept handle
    }

postgresSelectConcept :: DB.Handle -> Id Concept -> ExceptT ConceptRepositoryError IO Concept
postgresSelectConcept handle conceptId' = do
  maybeConcept <- runRepositoryQuery handle (DB.selectConcept conceptId')
  case maybeConcept of
    Nothing -> throwE $ ConceptNotFound ((pack . show) conceptId')    
    Just concept -> pure $ unserializeConcept concept 

postgresSelectAllConcepts :: DB.Handle -> ExceptT ConceptRepositoryError IO [(Id Concept, Concept)]
postgresSelectAllConcepts handle = do
    concepts <- runRepositoryQuery handle DB.selectAllConcepts
    pure $ map (\concept -> (conceptId concept, unserializeConcept concept)) concepts

postgresGetConceptByWikiLink :: DB.Handle -> Text -> ExceptT ConceptRepositoryError IO (Id Concept, Concept)
postgresGetConceptByWikiLink handle wikiLink = do
    concept <- runRepositoryQuery handle (DB.selectConceptByWikiLink wikiLink)
    case concept of
      Left _ -> throwE $ ConceptNotFound wikiLink
      Right concept' -> pure $ (conceptId concept', unserializeConcept concept')


postgresAddConcept :: DB.Handle -> Text -> Text -> Text -> ExceptT ConceptRepositoryError IO (Id Concept)
postgresAddConcept handle cName cDesc cWikiLink = do
  -- Generate a UUID for the concept
  conceptUUID <- liftIO nextRandom
  let query = Query.addConcept . litConcept $ serializeConcept (Id conceptUUID) (Concept cName cDesc cWikiLink)
  runRepositoryQuery handle query
  -- | after adding the concept, we now need to add the concept to the elo_scores table, with each model
  let allModelsQuery = Query.selectAllModels
  -- | get all the models
  -- | for each model, add a new elo entry for the given concept

  models <- runRepositoryQuery handle allModelsQuery
  mapM_ (\theModel -> do
          eloUUID <- liftIO nextRandom
          let eloQuery = Query.addElo . litElo $ serializeElo (Id eloUUID) $ Elo (Id conceptUUID) (modelId theModel) 1400 (UTCTime (ModifiedJulianDay 0) 0)
          runRepositoryQuery handle eloQuery
          ) models

  pure $ Id conceptUUID

-- | Run a query transforming a Hasql.QueryError into a ConceptRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT ConceptRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> ConceptRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "concepts_conceptWikiLink_key" `isInfixOf` message = DuplicateConceptWikiLink queryError
liftRepositoryError queryError = OtherError queryError

