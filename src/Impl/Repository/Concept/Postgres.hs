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


---- | For every model in the models table, add a new elo entry for the given concept, also gets the 
--addConceptToElo :: (Id Domain.Concept) -> Session ()
--addConceptToElo conceptId' = do
--  models <- selectAllModels
--  mapM_ (\(modelId', _) -> addElo $ Elo (Id Nothing) conceptId' modelId' 1400 (UTCTime (ModifiedJulianDay 0) 0)) models


--data Elo = Elo
--  { --  eloId :: UUID
--    conceptId :: Id Concept,
--    modelId :: Id Model,
--    eloScore :: Int32,
--    lastUpdate :: UTCTime
--    
--  }
--  deriving stock (Eq, Show, Generic)

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
  --
  --
  -- selectAllModels :: Session [Model Result]

  models <- runRepositoryQuery handle allModelsQuery
  mapM_ (\theModel -> do
          eloUUID <- liftIO nextRandom
          let eloQuery = Query.addElo . litElo $ serializeElo (Id eloUUID) $ Elo (Id conceptUUID) (modelId theModel) 1400 (UTCTime (ModifiedJulianDay 0) 0)
          runRepositoryQuery handle eloQuery
          ) models

  pure $ Id conceptUUID

--case models of
--    Left _ -> throwE $ OtherError (QueryError "Error getting all models" [] (ResultError (ServerError "23505" "Error getting all models" [] [])))
--    -- | we need to produce a new eloId for each model, thus we must call nextRandom again for length of models
--    Right models' -> do
--        -- | iterate over the models, generating a new eloId for each, then add the elo to the database
--        mapM_ (\(modelId', _) -> do
--          eloUUID <- liftIO nextRandom
--          let eloQuery = Query.addElo . litElo $ serializeElo (Id eloUUID) (Id conceptUUID) modelId' 1400 (UTCTime (ModifiedJulianDay 0) 0)
--          runRepositoryQuery handle eloQuery
--          ) models'
--

-- | Run a query transforming a Hasql.QueryError into a ConceptRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT ConceptRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> ConceptRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "concepts_conceptWikiLink_key" `isInfixOf` message = DuplicateConceptWikiLink queryError
liftRepositoryError queryError = OtherError queryError

-- |
-- A 'UserRepository' based on PostgreSQL
--repository :: DB.Handle -> UserRepository (ExceptT UserRepositoryError IO)
--repository handle =
--  UserRepository
--    { findByName = postgresGetUserByName handle,
--      add = postgresAddUser handle
--    }
--
--postgresGetUserByName :: DB.Handle -> Text -> ExceptT UserRepositoryError IO (Id User, User)
--postgresGetUserByName handle name = do
--  eitherUser <- runRepositoryQuery handle (Query.selectUserByName name)
--  case eitherUser of
--    Right usr -> pure (userId usr, unserializeUser usr)
--    Left e -> throwE $ UnexpectedNumberOfRows e

--postgresAddUser :: DB.Handle -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
--postgresAddUser handle name password = do
--  -- Generate the UUID for the user
--  userId' <- liftIO nextRandom
--  let query = Query.addUser . litUser $ serializeUser (Id userId') (User name password)
--
--  -- Actually add the user to the database, differentiating the `UserRepositoryError` cases
--  runRepositoryQuery handle query
--  pure $ Id userId'
--
--


-- | Run a query transforming a Hasql.QueryError into a UserRepositoryError as appropriate to the
-- domain.
--runRepositoryQuery :: DB.Handle -> Session a -> ExceptT UserRepositoryError IO a
--runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle
--
--liftRepositoryError :: QueryError -> UserRepositoryError
--liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
--  | "users_name_key" `isInfixOf` message = DuplicateUserName queryError
--liftRepositoryError queryError = OtherError queryError
