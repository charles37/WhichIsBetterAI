{-# LANGUAGE RankNTypes #-}

module API.AppServices where

import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Crypto.JOSE.JWK (JWK)
import Hasql.Session (QueryError)
import qualified Impl.Authentication.Authenticator as Auth
import Impl.Repository.Content as Repo.Content
import qualified Impl.Repository.User as Repo.User
import Impl.Repository.User.Error (UserRepositoryError (..))
import Infrastructure.Authentication.PasswordManager (PasswordManager, PasswordManagerError (..), bcryptPasswordManager)
import qualified Infrastructure.Authentication.PasswordManager as PasswordManager
import qualified Infrastructure.Database as DB
import Infrastructure.Logging.Logger (logError, logWarning, withContext)
import qualified Infrastructure.Logging.Logger as Logger
import Infrastructure.Persistence.Queries (WrongNumberOfResults (..))
import Servant (Handler, err401, err403, err500)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings)
import qualified Tagger.Authentication.Authenticator as Auth
import Tagger.Repository.Content (ContentRepository)
import qualified Tagger.Repository.Content as ContentRepository
import Tagger.Repository.User (UserRepository)
import qualified Tagger.Repository.User as UserRepository
import Prelude hiding (log)

import qualified Tagger.Repository.Concept as ConceptRepository
import Tagger.Repository.Concept (ConceptRepository)
import qualified Impl.Repository.Concept as Repo.Concept
import Impl.Repository.Concept.Error (ConceptRepositoryError (..))

import qualified Tagger.Repository.Model as ModelRepository
import Tagger.Repository.Model (ModelRepository)
import qualified Impl.Repository.Model as Repo.Model
import Impl.Repository.Model.Error (ModelRepositoryError (..))

import qualified Tagger.Repository.Elo as EloRepository
import Tagger.Repository.Elo (EloRepository)
import qualified Impl.Repository.Elo as Repo.Elo
import Impl.Repository.Elo.Error (EloRepositoryError (..))

import qualified Tagger.Repository.Comparison as ComparisonRepository
import Tagger.Repository.Comparison (ComparisonRepository)
import qualified Impl.Repository.Comparison as Repo.Comparison
import Impl.Repository.Comparison.Error (ComparisonRepositoryError (..))

-- |
-- Collection of services needed by the application to work
data AppServices = AppServices
  { jwtSettings :: JWTSettings,
    passwordManager :: PasswordManager Handler,
    contentRepository :: ContentRepository Handler,
    userRepository :: UserRepository Handler,
    authenticateUser :: Auth.Authenticator Handler,
    conceptRepository :: ConceptRepository Handler,
    modelRepository :: ModelRepository Handler,
    eloRepository :: EloRepository Handler,
    comparisonRepository :: ComparisonRepository Handler

  }

-- |
-- Lifts a computation from 'ExceptT e IO' to 'Handler a' using the provided 'handleError' function
eitherTToHandler :: (e -> Handler a) -> ExceptT e IO a -> Handler a
eitherTToHandler handleError = either handleError pure <=< liftIO . runExceptT

-- |
-- Lifts a 'ContentRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedContentRepository :: Logger.Handle -> ContentRepository (ExceptT QueryError IO) -> ContentRepository Handler
connectedContentRepository logHandle = ContentRepository.hoist (eitherTToHandler $ (>> throwError err500) . logError logHandle . show)

-- |
-- Lifts a 'UserRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedUserRepository :: Logger.Handle -> UserRepository (ExceptT UserRepositoryError IO) -> UserRepository Handler
connectedUserRepository logHandle = UserRepository.hoist $ eitherTToHandler handleUserRepositoryError
  where
    handleUserRepositoryError :: UserRepositoryError -> Handler a
    -- If the database error concerns a duplicate user, we return a 403 response
    handleUserRepositoryError (DuplicateUserName e) = do
      logWarning logHandle $ show (DuplicateUserName e)
      throwError err403
    -- Otherwise, we return a 500 response
    handleUserRepositoryError e = do
      logError logHandle (show e)
      throwError err500

-- | 
-- Lifts a 'ConceptRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedConceptRepository :: Logger.Handle -> ConceptRepository (ExceptT ConceptRepositoryError IO) -> ConceptRepository Handler
connectedConceptRepository logHandle = ConceptRepository.hoist $ eitherTToHandler handleConceptRepositoryError
    where 
      handleConceptRepositoryError :: ConceptRepositoryError -> Handler a
      -- If the database error concerns a duplicate concept, we return a 403 response
      handleConceptRepositoryError (DuplicateConceptWikiLink e) = do
        logWarning logHandle $ show (DuplicateConceptWikiLink e) 
        throwError err403
      -- Otherwise, we return a 500 response
      handleConceptRepositoryError e = do
        logError logHandle (show e)
        throwError err500

-- |
-- Lifts a 'ModelRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedModelRepository :: Logger.Handle -> ModelRepository (ExceptT ModelRepositoryError IO) -> ModelRepository Handler
connectedModelRepository logHandle = ModelRepository.hoist $ eitherTToHandler handleModelRepositoryError
    where 
      handleModelRepositoryError :: ModelRepositoryError -> Handler a
      -- If the database error concerns a duplicate model, we return a 403 response
      handleModelRepositoryError (DuplicateModelName e) = do
        logWarning logHandle $ show (DuplicateModelName e) 
        throwError err403
      -- Otherwise, we return a 500 response
      handleModelRepositoryError e = do
        logError logHandle (show e)
        throwError err500
-- |
-- Lifts a 'EloRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedEloRepository :: Logger.Handle -> EloRepository (ExceptT EloRepositoryError IO) -> EloRepository Handler
connectedEloRepository logHandle = EloRepository.hoist $ eitherTToHandler handleEloRepositoryError
    where 
      handleEloRepositoryError :: EloRepositoryError -> Handler a
      -- If the database error concerns a duplicate elo, we return a 403 response
--      handleEloRepositoryError (DuplicateElo e) = do
--        logWarning logHandle $ show (DuplicateElo e) 
--        throwError err403
      -- Otherwise, we return a 500 response
      handleEloRepositoryError e = do
        logError logHandle (show e)
        throwError err500

-- |
-- Lifts a 'ComparisonRepository' fo the 'Handler' monad, handling all errors by logging them and returning a 500 response
connectedComparisonRepository :: Logger.Handle -> ComparisonRepository (ExceptT ComparisonRepositoryError IO) -> ComparisonRepository Handler
connectedComparisonRepository logHandle = ComparisonRepository.hoist $ eitherTToHandler handleComparisonRepositoryError
    where 
      handleComparisonRepositoryError :: ComparisonRepositoryError -> Handler a
      -- If the database error concerns a duplicate comparison, we return a 403 response
     -- handleComparisonRepositoryError (DuplicateComparison e) = do
     --   logWarning logHandle $ show (DuplicateComparison e) 
     --   throwError err403
      -- Otherwise, we return a 500 response
      handleComparisonRepositoryError e = do
        logError logHandle (show e)
        throwError err500

-- |
-- Creates an 'AuthenticateUser' service injecting its dependencies and handling errors
connectedAuthenticateUser :: Logger.Handle -> UserRepository (ExceptT UserRepositoryError IO) -> PasswordManager Handler -> Auth.Authenticator Handler
connectedAuthenticateUser logHandle userRepository' passwordManager' =
  Auth.hoist
    (eitherTToHandler handleAuthenticationError)
    (Auth.authenticator userRepository' passwordManager')
  where
    handleAuthenticationError :: Auth.Error -> Handler a
    -- If the user was not found, we return a 401 response
    handleAuthenticationError (Auth.QueryError (UnexpectedNumberOfRows NoResults)) = do
      throwError err401
    -- If there was an error at the database level, we return a 500 response
    handleAuthenticationError (Auth.QueryError e) = do
      logError logHandle $ show (Auth.QueryError e)
      throwError err500
    -- In other cases, there was an authentication error and we return a 401 response
    handleAuthenticationError e = do
      logWarning logHandle (show e)
      throwError err401

-- |
-- Creates a 'PasswordManager' service injecting its dependencies and handling errors
encryptedPasswordManager :: Logger.Handle -> JWTSettings -> PasswordManager Handler
encryptedPasswordManager logHandle = PasswordManager.hoist (eitherTToHandler handlePasswordManagerError) . bcryptPasswordManager
  where
    handlePasswordManagerError :: PasswordManagerError -> Handler a
    -- If there was a failure during password hashing, we return a 500 response
    handlePasswordManagerError FailedHashing = do
      logError logHandle $ show FailedHashing
      throwError err500
    -- In other cases, we return a 401 response
    handlePasswordManagerError (FailedJWTCreation e) = do
      logError logHandle $ show (FailedJWTCreation e)
      throwError err401

start :: DB.Handle -> Logger.Handle -> JWK -> AppServices
start dbHandle logHandle key =
  let logContext = flip withContext logHandle
      passwordManager' = encryptedPasswordManager (withContext "PasswordManager" logHandle) $ defaultJWTSettings key
      dbUserRepository = Repo.User.postgres dbHandle
      dbConceptRepository = Repo.Concept.postgres dbHandle
      dbModelRepository = Repo.Model.postgres dbHandle
      dbEloRepository = Repo.Elo.postgres dbHandle  
      dbComparisonRepository = Repo.Comparison.postgres dbHandle
   in AppServices
        { jwtSettings = defaultJWTSettings key,
          passwordManager = passwordManager',
          contentRepository = connectedContentRepository (logContext "ContentRepository") (Repo.Content.postgres dbHandle),
          userRepository = connectedUserRepository (logContext "UserRepository") dbUserRepository,
          authenticateUser = connectedAuthenticateUser (logContext "AuthenticateUser") dbUserRepository passwordManager',
          conceptRepository = connectedConceptRepository (logContext "ConceptRepository") dbConceptRepository,
          modelRepository = connectedModelRepository (logContext "ModelRepository") dbModelRepository,
          eloRepository = connectedEloRepository (logContext "EloRepository") dbEloRepository,
          comparisonRepository = connectedComparisonRepository (logContext "ComparisonRepository") dbComparisonRepository
        }
