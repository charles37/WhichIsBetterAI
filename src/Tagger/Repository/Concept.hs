{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Concept where

import Data.Text (Text)
import Tagger.Concept (Concept)
import Tagger.Id (Id)

-- |
-- A 'ConceptRepository' represents a collection of 'Concept's.
-- It is indexed by a context 'm' which wraps the results.
data ConceptRepository m = ConceptRepository
  { -- | Searches the repository for 'Concept's with the provided name
    findByName :: Text -> m (Id Concept, Concept),
    -- | Adds a concept with the provided name and password
    add :: Text -> m (Id Concept)
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ConceptRepository' is operating
hoist :: (forall a. m a -> n a) -> ConceptRepository m -> ConceptRepository n
hoist f ConceptRepository {findByName, add} = ConceptRepository (f . findByName) (f . add) 
