{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Concept where

import Tagger.Concept (Concept)
import Tagger.Id (Id)
import Data.Text (Text)

---- |
---- A 'ContentRepository' represents a collection of 'Content's.
---- It is indexed by a context 'm' which wraps the results.
--data ContentRepository m = ContentRepository
--  { -- | selects all the 'Content's 'Owned' by a 'User' with a given 'Id' and indexed by all the provided 'Tag's
--    selectUserContentsByTags :: Id User -> [Tag] -> m [Owned (Content Tag)],
--    -- | adds a 'Content' indexed by some 'Tag's for a 'User' identified by a given 'Id'
--    addContentWithTags :: Id User -> Content Tag -> m (Id (Content Tag))
--  }
--
---- |
---- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ContentRepository' is operating
--hoist :: (forall a. m a -> n a) -> ContentRepository m -> ContentRepository n
--hoist f ContentRepository {selectUserContentsByTags, addContentWithTags} =
--  ContentRepository ((f .) . selectUserContentsByTags) ((f .) . addContentWithTags)
--
--
--
---- |
---- A 'UserRespository' represents a collection of 'User's.
---- It is indexed by a context 'm' which wraps the results.
--data UserRepository m = UserRepository
--  { -- | Searches the repository for 'User's with the provided name
--    findByName :: Text -> m (Id User, User),
--    -- | Adds a user with the provided name and password
--    add :: Text -> EncryptedPassword -> m (Id User)
--  }
--
---- |
---- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'UserRepository' is operating
--hoist :: (forall a. m a -> n a) -> UserRepository m -> UserRepository n
--hoist f UserRepository {findByName, add} = UserRepository (f . findByName) ((f .) . add)

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


