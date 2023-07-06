
{-# LANGUAGE TupleSections #-}

module Impl.Repository.Elo.Postgres (repository) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT, throwE)

--import Data.UUID.V4 (nextRandom)
import Data.ByteString (isInfixOf)
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as DB (selectAllElos) --
import Infrastructure.Persistence.Serializer (unserializeElo, unserializeConcept)
import Tagger.Elo (Elo)
import Tagger.Concept (Concept)
import Tagger.Model (Model) 
import Tagger.Id (Id) 
import Tagger.Repository.Elo (EloRepository (..))
import Data.Text (pack)
import qualified Infrastructure.Persistence.Queries as Query
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Impl.Repository.Elo.Error (EloRepositoryError (..))
import Infrastructure.Persistence.Schema (eloId, eloConceptId, eloScore)

import Data.Int (Int32)
import Data.List (sortBy)
import Data.Maybe (catMaybes)

--getEloByConceptAndModel :: (Id Domain.Model) -> (Id Domain.Concept) -> Session (Maybe (Elo Result))
--getEloByConceptAndModel modelId' conceptId' = statement () query
--  where
--    query = fmap listToMaybe . select $ do
--      elos <- each eloSchema
--      filter (\elo -> eloConceptId elo ==. lit conceptId' &&. eloModelId elo ==. lit modelId') elos 
--
--getEloByConcept :: (Id Domain.Concept) -> Session [Elo Result]
--getEloByConcept conceptId' = statement () query
--  where
--    query = select $ do
--      elos <- each eloSchema
--      filter (\elo -> eloConceptId elo ==. lit conceptId') elos
--
--getEloByModel :: (Id Domain.Model) -> Session [Elo Result]
--getEloByModel modelId' = statement () query
--  where
--    query = select $ do
--      elos <- each eloSchema
--      filter (\elo -> eloModelId elo ==. lit modelId') elos
--
--addElo :: Elo Expr -> Session ()
--addElo = statement () . add eloSchema . pure



--data EloRepository m = EloRepository
--  {
--    -- | selects a 'Elo' by its 'WikiLink' 
--    getEloScoreByConceptAndModel :: Id Model -> Id Concept -> m Int,
--
--    -- | get the leaderboard for a Model
--    getElosByModel :: Id Model -> m [Elo],
--
--    -- | see the difference in Elos for a particular Model
--    getElosByConcept :: Id Concept -> m [Elo],
--
--    -- | selects all the 'Elo's
--    selectAllElos :: m [(Id Elo, Elo)],
--    
--    updateScore :: Id Comparison -> m () 
--  }



--getEloByConceptAndModel :: (Id Domain.Model) -> (Id Domain.Concept) -> Session (Maybe (Elo Result))
--getEloByConceptAndModel modelId' conceptId' = statement (modelId', conceptId') query
--  where
--    query = fmap listToMaybe . select $ do
--      elos <- each eloSchema
--      filter (\elo -> conceptId elo ==. fst (param @1) &&. modelId elo ==. snd (param @1)) elos
--
--getElosByModel :: (Id Domain.Model) -> Session [Elo Result]
--getElosByModel modelId' = statement () query
--  where
--    query = select $ do
--      elos <- each eloSchema
--      filter (\elo -> modelId elo ==. lit modelId') elos
--
--getElosByConcept :: (Id Domain.Concept) -> Session [Elo Result]
--getElosByConcept conceptId' = statement () query
--  where
--    query = select $ do
--      elos <- each eloSchema
--      filter (\elo -> conceptId elo ==. lit conceptId') elos
--

-- |
-- A 'EloRepository' based on PostgreSQL
repository :: DB.Handle -> EloRepository (ExceptT EloRepositoryError IO)
repository handle =
  EloRepository
    { getEloScoreByConceptAndModel = postgresGetEloScoreByConceptAndModel handle,
      getElosByModel =  postgresGetElosByModel handle,
      getElosByConcept = postgresGetElosByConcept handle,
      getAllElos = postgresGetAllElos handle,
      getLeaderboard = postgresGetLeaderboard handle

    }

postgresGetAllElos :: DB.Handle -> ExceptT EloRepositoryError IO [(Id Elo, Elo)]
postgresGetAllElos handle = do
    elos <- runRepositoryQuery handle DB.selectAllElos
    pure $ map (\elo -> (eloId elo, unserializeElo elo)) elos

postgresGetEloScoreByConceptAndModel :: DB.Handle -> Id Model -> Id Concept -> ExceptT EloRepositoryError IO Int32
postgresGetEloScoreByConceptAndModel handle modelId conceptId = do
    elo <- runRepositoryQuery handle (Query.selectElosByConceptAndModel modelId conceptId)
    case elo of
      Nothing -> throwE $ EloNotFound $ "Elo not found for concept " <> (pack . show) conceptId <> " and model " <> (pack . show) modelId 
      Just el -> pure $ eloScore el
    
postgresGetElosByModel :: DB.Handle -> Id Model -> ExceptT EloRepositoryError IO [Elo]
postgresGetElosByModel handle modelId = do
    elos <- runRepositoryQuery handle (Query.selectElosByModel modelId)
    pure $ map unserializeElo elos

postgresGetElosByConcept :: DB.Handle -> Id Concept -> ExceptT EloRepositoryError IO [Elo]
postgresGetElosByConcept handle conceptId = do
    elos <- runRepositoryQuery handle (Query.selectElosByConcept conceptId)
    pure $ map unserializeElo elos

--getLeaderBoard gets the top 100 concepts by elo score for a given model and returns an array of tuples of the form (concept, eloScore) 
--
--selectLeaderboard :: (Id Domain.Model) -> Session [(Elo Result, Int32)]
--selectLeaderboard modelId' = statement () query
--  where
--    query = select $ do
--      elos <- each eloSchema
--      filter (\elo -> eloModelId elo ==. lit modelId') elos
--      orderBy (eloScore >$< desc)
--      limit 100
      

-- we need to query selectLeaderboard then query selectConcept

postgresGetLeaderboard :: DB.Handle -> Id Model -> ExceptT EloRepositoryError IO [(Concept, Int32)]
postgresGetLeaderboard handle modelId = do
    elos <- runRepositoryQuery handle (Query.selectElosByModel modelId)
    -- sort the elos by score and take the top 100
    -- This is actually a much harder problem in haskell than youd think
    -- https://ro-che.info/articles/2016-04-02-descending-sort-haskell 
    let sortedElos = sortBy (\a b -> compare (eloScore b) (eloScore a)) elos 
    let top100 = take 100 sortedElos
    concepts <- mapM (runRepositoryQuery handle . Query.selectConcept . eloConceptId) top100 
    let conceptScores = zip (catMaybes concepts) (map eloScore top100)
    pure $ map (\(concept, score) -> (unserializeConcept concept, score)) conceptScores

---- | For every model in the models table, add a new elo entry for the given concept, also gets the 
--addConceptToElo :: (Id Domain.Concept) -> Session ()
--addConceptToElo conceptId' = do
--  models <- selectAllModels
--  mapM_ (\(modelId', _) -> addElo $ Elo (Id Nothing) conceptId' modelId' 1400 (UTCTime (ModifiedJulianDay 0) 0)) models





--data Comparison = Comparison
--  { --  comparisonID :: UUID
--    concept1Id :: Id Concept,
--    concept2Id :: Id Concept,
--    concept1EloBefore :: Int,
--    concept2EloBefore :: Int,
--    concept1EloAfter :: Int,
--    concept2EloAfter :: Int,
--    winning_conceptId :: Id Concept,
--    modelId :: Id Model,
--    comparisonTimestamp :: UTCTime 
--  }
--  deriving stock (Eq, Show, Generic)


--postgresUpdateScore :: DB.Handle -> Id Comparison -> ExceptT EloRepositoryError IO ()
--postgresUpdateScore handle comparisonId = do
--    comparison <- runRepositoryQuery handle (DB.selectComparison comparisonId)
--    case comparison of
--      Nothing -> throwE $ NoSuchComparison comparisonId
--      Just comparison -> do
--        let concept1Id' = concept1Id comparison
--        let concept2Id' = concept2Id comparison
--        let concept1EloBefore' = concept1EloBefore comparison
--        let concept2EloBefore' = concept2EloBefore comparison
--        let concept1EloAfter' = concept1EloAfter comparison
--        let concept2EloAfter' = concept2EloAfter comparison
--        let winning_conceptId' = winning_conceptId comparison
--        let modelId' = modelId comparison
--        let comparisonTimestamp' = comparisonTimestamp comparison
--        runRepositoryQuery handle (DB.updateElo concept1Id' concept1EloAfter' modelId')
--        runRepositoryQuery handle (DB.updateElo concept2Id' concept2EloAfter' modelId')


-- | Run a query transforming a Hasql.QueryError into a EloRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT EloRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> EloRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "elos_elo_name_key" `isInfixOf` message = DuplicateEloName queryError
liftRepositoryError queryError = OtherError queryError





