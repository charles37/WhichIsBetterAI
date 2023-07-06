
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


-- | Run a query transforming a Hasql.QueryError into a EloRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT EloRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> EloRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "elos_elo_name_key" `isInfixOf` message = DuplicateEloName queryError
liftRepositoryError queryError = OtherError queryError





