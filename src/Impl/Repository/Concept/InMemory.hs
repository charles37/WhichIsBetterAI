{-# LANGUAGE OverloadedStrings #-}

module Impl.Repository.Concept.InMemory (Table, repository) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Lazy (Map, insert)
import GHC.Conc (TVar, atomically, readTVar, readTVarIO, writeTVar)
import Tagger.Concept (Concept(..))
import Tagger.Id (Id(..))
import Tagger.Repository.Concept (ConceptRepository(..))
import Prelude hiding (filter)

type Table = TVar (Map (Id Concept) Concept)

repository :: Table -> ConceptRepository (ExceptT ConceptRepositoryError IO)
repository conceptMap =
  ConceptRepository
    { findByConceptName = inMemoryGetConceptByName conceptMap,
      addConcept = inMemoryAddConcept conceptMap
    }

-- Fill in implementations of inMemoryGetConceptByName and inMemoryAddConcept
-- similar to those found in User.InMemory.hs


