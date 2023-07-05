module TestServices where

import API.AppServices (AppServices (..), connectedAuthenticateUser, connectedContentRepository, connectedUserRepository, encryptedPasswordManager)
import GHC.Conc (newTVarIO)
import qualified Impl.Repository.Content as Repo.Content
import qualified Impl.Repository.User as Repo.User
import Infrastructure.Logging.Logger as Logger
import Infrastructure.SystemTime as SystemTime
import Servant.Auth.Server (defaultJWTSettings, generateKey)

testServices :: IO AppServices
testServices = do
  key <- generateKey
  userMap <- newTVarIO mempty
  contentsMap <- newTVarIO mempty
  SystemTime.withHandle $ \timeHandle ->
    Logger.withHandle timeHandle $ \loggerHandle -> do
      let passwordManager' = encryptedPasswordManager loggerHandle $ defaultJWTSettings key
      let userRepository' = Repo.User.inMemory userMap
      let contentsRepository = Repo.Content.inMemory contentsMap
      pure $
        AppServices
          { jwtSettings = defaultJWTSettings key,
            passwordManager = passwordManager',
            contentRepository = connectedContentRepository loggerHandle contentsRepository,
            userRepository = connectedUserRepository loggerHandle userRepository',
            authenticateUser = connectedAuthenticateUser loggerHandle userRepository' passwordManager'
          }



---- Table for Concepts
--CREATE TABLE concepts (
--    concept_id SERIAL PRIMARY KEY,
--    concept_name VARCHAR(255) NOT NULL,
--    concept_description TEXT NOT NULL,
--    concept_wiki_link TEXT NOT NULL
--);
--
---- Table for AI Models
--CREATE TABLE ai_models (
--    model_id SERIAL PRIMARY KEY,
--    model_name VARCHAR(255) NOT NULL,
--    model_description TEXT NOT NULL
--);
--
---- Table for Comparison Results
--CREATE TABLE comparison_results (
--    comparison_id SERIAL PRIMARY KEY,
--    concept1_id INTEGER NOT NULL,
--    concept2_id INTEGER NOT NULL,
--    winning_concept_id INTEGER NOT NULL,
--    model_id INTEGER NOT NULL,
--    comparison_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
--    FOREIGN KEY (concept1_id) REFERENCES concepts (concept_id),
--    FOREIGN KEY (concept2_id) REFERENCES concepts (concept_id),
--    FOREIGN KEY (winning_concept_id) REFERENCES concepts (concept_id),
--    FOREIGN KEY (model_id) REFERENCES ai_models (model_id)
--);
--
---- Table for ELO Scores
--CREATE TABLE elo_scores (
--    elo_id SERIAL PRIMARY KEY,
--    concept_id INTEGER NOT NULL,
--    model_id INTEGER NOT NULL,
--    elo_score INTEGER NOT NULL DEFAULT 0,
--    last_update_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
--    FOREIGN KEY (concept_id) REFERENCES concepts (concept_id),
--    FOREIGN KEY (model_id) REFERENCES ai_models (model_id)
--);

-- | Test for AIComparisonService

--testAIComparisonService :: IO AppServices
--testAIComparisonService = do
--  key <- generateKey
--  userMap <- newTVarIO mempty
--  contentsMap <- newTVarIO mempty
--  conceptMap <- newTVarIO mempty
--  aiModelsMap <- newTVarIO mempty
--  comparisonResultsMap <- newTVarIO mempty
--  eloScoresMap <- newTVarIO mempty
--  SystemTime.withHandle $ \timeHandle ->
--    Logger.withHandle timeHandle $ \loggerHandle -> do
--      let passwordManager' = encryptedPasswordManager loggerHandle $ defaultJWTSettings key
--      let userRepository' = Repo.User.inMemory userMap
--      let conceptRepository' = Repo.Concept.inMemory conceptMap
--      let aiModelsRepository' = Repo.AIModel.inMemory aiModelsMap
--      let comparisonResultsRepository' = Repo.Comparison.inMemory comparisonResultsMap
--      let eloScoresRepository' = Repo.Elo.inMemory eloScoresMap
--      pure $
--        AppServices
--          { jwtSettings = defaultJWTSettings key,
--            passwordManager = passwordManager',
--            contentRepository = connectedContentRepository loggerHandle conceptRepository',
--            userRepository = connectedUserRepository loggerHandle userRepository',
--            authenticateUser = connectedAuthenticateUser loggerHandle userRepository' passwordManager'
--
--          }
--
--
