{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Impl.Repository.Comparison.Postgres (repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), withExceptT, throwE)

import Data.UUID.V4 (nextRandom)
import Data.ByteString (isInfixOf)
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Persistence.Queries as DB (selectAllComparisons, selectComparison) --
import Infrastructure.Persistence.Serializer (serializeComparison, unserializeComparison)
import Tagger.Comparison (Comparison (Comparison))
import Tagger.Concept (Concept)
import Tagger.Model (Model)

import Tagger.Id (Id (Id))
import Tagger.Repository.Comparison (ComparisonRepository (..))
import Data.Text (pack)
import qualified Infrastructure.Persistence.Queries as Query
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError), Session)
import Impl.Repository.Comparison.Error (ComparisonRepositoryError (..))
import Infrastructure.Persistence.Schema (litComparison, comparisonId, conceptId, eloScore, conceptName, modelName, modelId)

import Data.Time.Clock (getCurrentTime)
import Data.Int (Int32)

import System.Random (randomRIO)


-- runComparison :: ModelName -> ConceptName -> ConceptName -> ConceptName 
-- runComparison :: Text -> Text -> Text -> ExceptT ComparisonRepositoryError IO Text 
import Tagger.AI (runComparison)
import Tagger.Scoring (calculateELOWinnerScoreFirst)

--data ComparisonRepository m = ComparisonRepository
--  {
--    -- | select a comparison by 'Id'
--    selectComparison :: Id Comparison -> m Comparison,
--    
--    selectAllComparisons :: m [(Id Comparison, Comparison)],
--    -- | adds a 'Comparison'
--
--    --addComparison :: Id Concept -> Id Concept -> Int -> Int -> Int -> Int -> Id Model -> m (Id Comparison),
--
--    doComparisonSingle :: Id Model -> Id Concept -> Id Concept -> m (Id Comparison, Comparison),
--
--    doComparisonAllModels :: Id Concept -> Id Concept -> m [(Id Comparison, Comparison)],
--
--    runRandomComparisons :: Int32 -> m [(Id Comparison, Comparison)]
--  }

--data Comparison = Comparison
--  { --  comparisonID :: UUID
--    concept1Id :: Id Concept,
--    concept2Id :: Id Concept,
--    concept1EloBefore :: Int32,
--    concept2EloBefore :: Int32,
--    concept1EloAfter :: Int32,
--    concept2EloAfter :: Int32,
--    theWinningConceptId :: Id Concept,
--    comparisonModelId :: Id Model,
--    comparisonTimestamp :: UTCTime 
--  }
--  deriving stock (Eq, Show, Generic)

-- |
-- A 'ComparisonRepository' based on PostgreSQL
repository :: DB.Handle -> ComparisonRepository (ExceptT ComparisonRepositoryError IO)
repository handle =
  ComparisonRepository
    { selectComparison = postgresSelectComparison handle,
      selectAllComparisons = postgresSelectAllComparisons handle, 
      doComparisonSingle = postgresDoComparisonSingle handle,
      doComparisonAllModels = postgresDoComparisonAllModels handle,
      runRandomComparisons = postgresRunRandomComparisons handle

    }

postgresSelectComparison :: DB.Handle -> Id Comparison -> ExceptT ComparisonRepositoryError IO Comparison
postgresSelectComparison handle comparisonId' = do
  maybeComparison <- runRepositoryQuery handle (DB.selectComparison comparisonId')
  case maybeComparison of
    Nothing -> throwE $ ComparisonNotFound ((pack . show) comparisonId')    
    Just comparison -> pure $ unserializeComparison comparison 

postgresSelectAllComparisons :: DB.Handle -> ExceptT ComparisonRepositoryError IO [(Id Comparison, Comparison)]
postgresSelectAllComparisons handle = do
    comparisons <- runRepositoryQuery handle DB.selectAllComparisons
    pure $ map (\comparison -> (comparisonId comparison, unserializeComparison comparison)) comparisons

postgresDoComparisonSingle :: DB.Handle -> Id Model -> Id Concept -> Id Concept -> ExceptT ComparisonRepositoryError IO (Id Comparison, Comparison) 
postgresDoComparisonSingle handle modelId' concept1Id' concept2Id' = do
  -- Generate the UUID for the comparison
  comparisonId' <- liftIO nextRandom
  -- get the model Name
  
--selectModel :: (Id Domain.Model) -> Session (Maybe (Model Result))

  let getModelNameQuery = Query.selectModel modelId'
  modelObjectResult <- runRepositoryQuery handle getModelNameQuery

  --let modelName = case modelObjectResult of
  --      Nothing -> throwE $ ModelNotFound ((pack . show) modelId')
  --      Just model -> modelName model
  
  let getConceptNameQuery = Query.selectConcept 
  
  concept1ObjectResult <- runRepositoryQuery handle (getConceptNameQuery concept1Id')

  --let concept1Name = case concept1ObjectResult of
  --      Nothing -> throwE $ ConceptNotFound ((pack . show) concept1Id')
  --      Just concept -> conceptName concept


  concept2ObjectResult <- runRepositoryQuery handle (getConceptNameQuery concept2Id')

  --let concept2Name = case concept2ObjectResult of
  --      Nothing -> throwE $ ConceptNotFound ((pack . show) concept2Id')
  --      Just concept -> conceptName concept
    

  case (modelObjectResult, concept1ObjectResult, concept2ObjectResult) of
    (Nothing, _, _) -> throwE $ ModelNotFound ((pack . show) modelId')
    (_, Nothing, _) -> throwE $ ConceptNotFound ((pack . show) concept1Id')
    (_, _, Nothing) -> throwE $ ConceptNotFound ((pack . show) concept2Id')
    (Just model, Just concept1, Just concept2) -> do
      let modelName' = modelName model
      let concept1Name = conceptName concept1
      let concept2Name = conceptName concept2
              

      winningConceptName <- liftIO $ runComparison modelName' concept1Name concept2Name  


      case (winningConceptName == concept1Name, winningConceptName == concept2Name) of
             (False, False) -> throwE $ ComparisonError ("The winning concept is neither " <> concept1Name <> " nor " <> concept2Name <> " got: " <> winningConceptName) 
             (True, True) -> throwE $ ComparisonError ("The winning concept is both " <> concept1Name <> " and " <> concept2Name <> " got: " <> winningConceptName)
             _ -> do
                let theWinningConceptId = if winningConceptName == concept1Name then concept1Id' else concept2Id'
             
              --getEloByConceptAndModel :: (Id Domain.Model) -> (Id Domain.Concept) -> Session (Maybe (Elo Result))
              --getEloByConceptAndModel modelId' conceptId' = statement () query
               -- where
               --   query = fmap listToMaybe . select $ do
               --     elos <- each eloSchema
               --     filter (\elo -> eloConceptId elo ==. lit conceptId' &&. eloModelId elo ==. lit modelId') elos 

                let eloScoreQuery = Query.selectElosByConceptAndModel modelId'

                concept1EloBeforeResult <- runRepositoryQuery handle (eloScoreQuery concept1Id')
                --concept1EloBefore' <- case concept1EloBeforeResult of
                --      Nothing -> throwE $ EloNotFound ((pack . show) concept1Id')
                --      Just elo -> pure $ eloScore elo 

                concept2EloBeforeResult <- runRepositoryQuery handle (eloScoreQuery concept2Id')

                --concept2EloBefore' <- case concept2EloBeforeResult of
                --      Nothing -> throwE $ EloNotFound ((pack . show) concept2Id')
                --      Just elo -> pure $ eloScore elo

                case (concept1EloBeforeResult, concept2EloBeforeResult) of
                  (Nothing, _) -> throwE $ EloNotFound ((pack . show) concept1Id')
                  (_, Nothing) -> throwE $ EloNotFound ((pack . show) concept2Id')
                  (Just concept1EloBefore, Just concept2EloBefore) -> do
                    let concept1EloBefore' = eloScore concept1EloBefore
                    let concept2EloBefore' = eloScore concept2EloBefore

                    let (concept1EloAfter', concept2EloAfter') = if theWinningConceptId == concept1Id' 
                                                                  then calculateELOWinnerScoreFirst concept1EloBefore' concept2EloBefore'
                                                                  else calculateELOWinnerScoreFirst concept2EloBefore' concept1EloBefore'
                    curTime <- liftIO $ getCurrentTime 

                    -- Now we have to adjust ELO Scores for the concepts
                  
        --up  dateScore :: (Id Domain.Model) -> (Id Domain.Concept) -> Int32 -> Session ()
                    let updateEloQuery = Query.updateScore modelId'

                    runRepositoryQuery handle (updateEloQuery concept1Id' concept1EloAfter')

                    runRepositoryQuery handle (updateEloQuery concept2Id' concept2EloAfter')

                    let query = Query.addComparison . litComparison $ serializeComparison (Id comparisonId') $ Comparison concept1Id' concept2Id' concept1EloBefore' concept2EloBefore' concept1EloAfter' concept2EloAfter' theWinningConceptId modelId' curTime
                    -- Actually add the comparison to the database, differentiating the `ComparisonRepositoryError` cases
                    runRepositoryQuery handle query
                    pure $ (Id comparisonId', Comparison concept1Id' concept2Id' concept1EloBefore' concept2EloBefore' concept1EloAfter' concept2EloAfter' theWinningConceptId modelId' curTime) 

-- | postgresDoComparisonAllModels does a comparison for the two given concepts for all models in the database
postgresDoComparisonAllModels :: DB.Handle -> Id Concept -> Id Concept -> ExceptT ComparisonRepositoryError IO [(Id Comparison, Comparison)]
postgresDoComparisonAllModels handle concept1Id' concept2Id' = do

-- Get the models
  let getModelsQuery = Query.selectAllModels

  models <- runRepositoryQuery handle getModelsQuery

  -- call postgresDoComparisonSingle for each model

  case models of
    [] -> throwE $ NoModelsInDatabase
    _ -> do
        let doComparisonForModel model = postgresDoComparisonSingle handle (modelId model) concept1Id' concept2Id'
        comparisons <- mapM doComparisonForModel models
        pure comparisons


-- | postgresRunRandomComparisons does gets two random distinct concepts and does a comparison for them 
-- | it then gets two more random distinct concepts and does a comparison for them by running postgresDoComparisonAllModels
-- | it keeps getting random distinct concepts and doing comparisons until it has done n comparisons
postgresRunRandomComparisons :: DB.Handle -> Int32 -> ExceptT ComparisonRepositoryError IO [(Id Comparison, Comparison)]
postgresRunRandomComparisons handle n = do
    let getConceptsQuery = Query.selectAllConcepts
    
    concepts <- runRepositoryQuery handle getConceptsQuery
    
    case concepts of
        [] -> throwE $ NoConceptsInDatabase
        _ -> do
            let doComparisonForConcepts concept1 concept2 = postgresDoComparisonAllModels handle (conceptId concept1) (conceptId concept2)
            comparisons <- mapM (\_ -> do
                -- pickRandomElement :: [a] -> IO (Maybe a)
                concept1 <- liftIO $ pickRandomElement concepts
                concept2 <- liftIO $ pickRandomElement concepts
                --Wonder what happens if they are the same concept?
                doComparisonForConcepts concept1 concept2) [1..n] 
            pure $ concat comparisons
    
pickRandomElement :: [a] -> IO a
pickRandomElement list = do
    index <- randomRIO (0, length list - 1)
    return (list !! index)

-- | Run a query transforming a Hasql.QueryError into a ComparisonRepositoryError as appropriate to the
-- domain.

runRepositoryQuery :: DB.Handle -> Session a -> ExceptT ComparisonRepositoryError IO a
runRepositoryQuery handle = withExceptT liftRepositoryError . ExceptT . DB.runQuery handle

liftRepositoryError :: QueryError -> ComparisonRepositoryError
liftRepositoryError queryError@(QueryError _ _ (ResultError (ServerError "23505" message _ _)))
  | "comparisons_comparison_name_key" `isInfixOf` message = DuplicateComparisonName queryError
liftRepositoryError queryError = OtherError queryError


