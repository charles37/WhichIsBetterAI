{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Concept where

import Tagger.Concept (Concept)
import Tagger.Id (Id)
import Data.Text (Text)

-- |
-- A 'ConceptRepository' represents a collection of 'Concept's.
-- It is indexed by a context 'm' which wraps the results.
data ConceptRepository m = ConceptRepository
  { -- | selects a 'Concept' by its 'Id'
    selectConcept :: Id Concept -> m Concept,
    -- | selects a 'Concept' by its 'WikiLink' 
    getConceptByWikiLink :: Text -> m (Id Concept, Concept),
    -- | selects all the 'Concept's
    selectAllConcepts :: m [(Id Concept, Concept)],
    -- | adds a 'Concept'
    addConcept :: Text -> Text -> Text -> m (Id Concept)
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ConceptRepository' is operating
hoist :: (forall a. m a -> n a) -> ConceptRepository m -> ConceptRepository n
hoist f ConceptRepository {selectConcept, getConceptByWikiLink, selectAllConcepts, addConcept} =
    ConceptRepository (f . selectConcept) (f . getConceptByWikiLink) (f selectAllConcepts) (((f .) .) . addConcept) 


