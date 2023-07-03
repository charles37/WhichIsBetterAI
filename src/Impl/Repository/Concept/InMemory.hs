{-# LANGUAGE OverloadedStrings #-}

module Impl.Repository.Concept.InMemory (Table, repository) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.UUID.V4 (nextRandom)
import Tagger.Concept (Concept (..))
import Tagger.Id (Id (Id))
import Tagger.Repository.Concept (ConceptRepository (..))
import Impl.Repository.Concept.Error (ConceptRepositoryError (..))
import qualified Data.Map as Map 
import Data.Text (Text, pack)

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, assocs, filter, insert, size)
import Data.Text.Encoding (encodeUtf8)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Hasql.Session (CommandError (ResultError), QueryError (QueryError), ResultError (ServerError))
import PostgreSQL.ErrorCodes (unique_violation)
import Prelude hiding (filter)

type Table = TVar (Map (Id Concept) Concept)

--data Concept = Concept
--  { --  conceptId :: UUID
--    conceptName :: Text,
--    conceptDescription :: Text,
--    conceptWikiLink :: Text
--  }
--  deriving stock (Eq, Show, Generic)

-- |
--
--repository :: DB.Handle -> ConceptRepository (ExceptT ConceptRepositoryError IO)
--repository handle =
--  ConceptRepository
--    { selectConcept = postgresSelectConcept handle,
--      selectAllConcepts = postgresSelectAllConcepts handle,
--      getConceptByWikiLink = postgresGetConceptByWikiLink handle,
--      addConcept = postgresAddConcept handle
--    }


--repository :: Table -> UserRepository (ExceptT UserRepositoryError IO)
--repository userMap =
--  UserRepository
--    { findByName = inMemoryGetUserByName userMap,
--      add = inMemoryAddUser userMap
--    }
--
--inMemoryGetUserByName :: Table -> Text -> ExceptT UserRepositoryError IO (Id User, User)
--inMemoryGetUserByName userMap name' = do
--  users <- liftIO $ readTVarIO userMap
--  let usersWithName = filter ((== name') . name) users
--  case size usersWithName of
--    0 -> throwError $ UnexpectedNumberOfRows NoResults
--    1 -> pure . head . assocs $ usersWithName
--    _ -> throwError $ UnexpectedNumberOfRows MoreThanOneResult
--
--duplicateNameError :: Text -> UserRepositoryError
--duplicateNameError name' =
--  DuplicateUserName $
--    QueryError
--      "insert user"
--      []
--      ( ResultError $
--          ServerError
--            unique_violation
--            "duplicate key value violates unique constraint"
--            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
--            Nothing
--      )
--
--inMemoryAddUser :: Table -> Text -> EncryptedPassword -> ExceptT UserRepositoryError IO (Id User)
--inMemoryAddUser userMap name' password' = do
--  userId <- Id <$> liftIO nextRandom
--  queryError <- liftIO . atomically $ do
--    users <- readTVar userMap
--    let usersWithName = filter ((== name') . name) users
--    if null usersWithName
--      then writeTVar userMap (insert userId (User name' password') users) >> pure Nothing
--      else pure . Just $ duplicateNameError name'
--  case queryError of
--    Just qe -> throwError qe
--    Nothing -> pure userId

repository :: Table -> ConceptRepository (ExceptT ConceptRepositoryError IO)
repository conceptsMap =
  ConceptRepository
    { selectConcept = inMemorySelectConcept conceptsMap,
      selectAllConcepts = inMemorySelectAllConcepts conceptsMap,
      getConceptByWikiLink = inMemoryGetConceptByWikiLink conceptsMap,
      addConcept = inMemoryAddConcept conceptsMap
    }

inMemorySelectConcept :: TVar (Map (Id Concept) Concept) -> Id Concept -> ExceptT ConceptRepositoryError IO Concept
inMemorySelectConcept conceptsMap conceptId = do
  concepts <- liftIO $ readTVarIO conceptsMap
  case Map.lookup conceptId concepts of
    Nothing -> throwError $ ConceptNotFound (pack . show $ conceptId)
    Just concept -> pure concept

inMemoryGetConceptByWikiLink :: TVar (Map (Id Concept) Concept) -> Text -> ExceptT ConceptRepositoryError IO (Id Concept, Concept)
inMemoryGetConceptByWikiLink conceptsMap wikiLink = do
  concepts <- liftIO $ readTVarIO conceptsMap
  let conceptsWithWikiLink = filter ((== wikiLink) . conceptWikiLink) concepts
  case size conceptsWithWikiLink of
    0 -> throwError $ ConceptNotFoundByWikiLink ("WikiLink: " <> wikiLink) 
    1 -> pure . head . assocs $ conceptsWithWikiLink 
    _ -> throwError $ duplicateWikiLinkError wikiLink 


inMemorySelectAllConcepts :: TVar (Map (Id Concept) Concept) -> ExceptT ConceptRepositoryError IO [(Id Concept, Concept)]
inMemorySelectAllConcepts conceptsMap = do
    concepts <- liftIO $ readTVarIO conceptsMap
    pure $ assocs concepts


inMemoryAddConcept :: TVar (Map (Id Concept) Concept) -> Text -> Text -> Text -> ExceptT ConceptRepositoryError IO (Id Concept)
inMemoryAddConcept conceptsMap cName cDesc cWikiLink = do
  conceptId <- Id <$> liftIO nextRandom
  queryError <- liftIO . atomically $ do
    concepts <- readTVar conceptsMap
    let conceptsWithWikiLink = filter ((== cWikiLink) . conceptWikiLink) concepts
    if null conceptsWithWikiLink
        then writeTVar conceptsMap (insert conceptId (Concept cName cDesc cWikiLink) concepts) >> pure Nothing
        else pure . Just $ duplicateWikiLinkError cWikiLink
  case queryError of
    Just qe -> throwError qe
    Nothing -> pure conceptId

duplicateWikiLinkError :: Text -> ConceptRepositoryError
duplicateWikiLinkError wikiLink =
  DuplicateConceptWikiLink $
    QueryError
      "insert concept"
      []
      ( ResultError $
          ServerError
            unique_violation
            "duplicate key value violates unique constraint"
            (Just $ "Key (wikiLink)=(" <> encodeUtf8 wikiLink <> ") already exists")
            Nothing
      )
--duplicateNameError :: Text -> UserRepositoryError
--duplicateNameError name' =
--  DuplicateUserName $
--    QueryError
--      "insert user"
--      []
--      ( ResultError $
--          ServerError
--            unique_violation
--            "duplicate key value violates unique constraint"
--            (Just $ "Key (name)=(" <> encodeUtf8 name' <> ") already exists")
--            Nothing
--      )
