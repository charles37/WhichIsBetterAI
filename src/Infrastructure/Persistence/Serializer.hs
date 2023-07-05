module Infrastructure.Persistence.Serializer where

import Infrastructure.Persistence.Schema (contentContent, contentId, contentUserId, tagId, tagName, userId, userName, userPassword, 
    conceptId, conceptName, conceptDescription, conceptWikiLink, modelId, modelName, modelDescription, comparisonId, concept1Id, concept2Id,
    concept1EloBefore, concept2EloBefore, concept1EloAfter, concept2EloAfter, winningConceptId,
    comparisonModelId, comparisonTimestamp, eloId, eloConceptId, eloModelId, eloScore, eloLastUpdate)
import qualified Infrastructure.Persistence.Schema as DB (Content (Content), Tag (Tag), User (User),
    Concept (Concept), Model (Model), Comparison (Comparison), Elo (Elo))
import Rel8 (Result)
import Tagger.Content (Content (..), createContent)
import Tagger.Id (Id)
import Tagger.Owned (Owned (Owned))
import qualified Tagger.Owned as Owned (content, userId)
import Tagger.Tag (Tag (Tag))
import qualified Tagger.Tag as Tag (name)
import Tagger.User (User (User))
import qualified Tagger.User as User (name, password)

import Tagger.Concept (Concept (Concept))
import qualified Tagger.Concept as Concept (conceptName, conceptDescription, conceptWikiLink)
import Tagger.Model (Model (Model))
import qualified Tagger.Model as Model (modelName, modelDescription)
import Tagger.Comparison (Comparison (Comparison))
import qualified Tagger.Comparison as Comparison (concept1Id, concept2Id, concept1EloBefore, concept2EloBefore, concept1EloAfter, concept2EloAfter, winningConceptId, comparisonModelId, comparisonTimestamp)
import Tagger.Elo (Elo (Elo))
import qualified Tagger.Elo as Elo (eloConceptId, eloModelId, eloScore, eloLastUpdate)



-- CONTENT

-- |
-- Transform from a domain representation of a 'Content' to its underlying database representation
serializeContent :: Id (Content Tag) -> Id User -> Content (Id Tag, Tag) -> (DB.Content Result, [DB.Tag Result])
serializeContent contentId' userId' content = (dbContent, dbTags)
  where
    dbContent =
      DB.Content
        { contentId = contentId',
          contentContent = message content,
          contentUserId = userId'
        }
    dbTags = uncurry serializeTag <$> tags content

-- |
-- Transform from the database representation of a 'Content' to its domain representation
unserializeContent :: DB.Content Result -> [DB.Tag Result] -> DB.User Result -> Owned (Content Tag)
unserializeContent content tags' user =
  Owned
    { Owned.content =
        createContent
          (contentContent content)
          (unserializeTag <$> tags'),
      Owned.userId = userId user
    }

-- TAG

-- |
-- Transform from a domain representation of a 'Tag' to its underlying database representation
serializeTag :: Id Tag -> Tag -> DB.Tag Result
serializeTag uuid tag =
  DB.Tag
    { tagId = uuid,
      tagName = Tag.name tag
    }

-- |
-- Transform from the database representation of a 'Tag' to its domain representation
unserializeTag :: DB.Tag Result -> Tag
unserializeTag tag = Tag (tagName tag)

-- USER

-- |
-- Transform from a domain representation of a 'User' to its underlying database representation
serializeUser :: Id User -> User -> DB.User Result
serializeUser uuid user =
  DB.User
    { userId = uuid,
      userName = User.name user,
      userPassword = User.password user
    }

-- |
-- Transform from the database representation of a 'User' to its domain representation
unserializeUser :: DB.User Result -> User
unserializeUser user = User (userName user) (userPassword user)



--data Concept f = Concept
--  { conceptId :: Column f (Id Concept.Concept),
--    conceptName :: Column f Text,
--    conceptDescription :: Column f Text,
--    conceptWikiLink :: Column f Text
--  }
--  deriving stock (Generic)
--  deriving anyclass (Rel8able)


-- CONCEPT 
-- |
-- Transform from a domain representation of a 'Concept' to its underlying database representation

serializeConcept :: Id Concept -> Concept -> DB.Concept Result
serializeConcept conceptId' concept = 
  DB.Concept
    { conceptId = conceptId',
      conceptName = Concept.conceptName concept,
      conceptDescription = Concept.conceptDescription concept,
      conceptWikiLink = Concept.conceptWikiLink concept
    }

-- |
-- Transform from the database representation of a 'Concept' to its domain representation

unserializeConcept :: DB.Concept Result -> Concept
unserializeConcept concept = Concept (conceptName concept) (conceptDescription concept) (conceptWikiLink concept)


--data Model f = Model
--  { modelId :: Column f (Id Domain.Model),
--    modelName :: Column f Text,
--    modelDescription :: Column f Text
--  }
--  deriving stock (Generic)
--  deriving anyclass (Rel8able)


-- MODEL
-- |
-- Transform from a domain representation of a 'Model' to its underlying database representation

serializeModel :: Id Model -> Model -> DB.Model Result
serializeModel modelId' model = 
  DB.Model
    { modelId = modelId',
      modelName = Model.modelName model,
      modelDescription = Model.modelDescription model
    }

-- |
-- Transform from the database representation of a 'Model' to its domain representation
--
--
unserializeModel :: DB.Model Result -> Model
unserializeModel model = Model (modelName model) (modelDescription model)


--data Comparison = Comparison
--  { --  comparisonID :: UUID
--    concept1Id :: Id Concept,
--    concept2Id :: Id Concept,
--    concept1EloBefore :: Int32,
--    concept2EloBefore :: Int32,
--    concept1EloAfter :: Int32,
--    concept2EloAfter :: Int32,
--    winningConceptId :: Id Concept,
--    comparisonModelId :: Id Model,
--    comparisonTimestamp :: UTCTime 
--  }
--  deriving stock (Eq, Show, Generic)

-- COMPARISON
-- |
-- Transform from a domain representation of a 'Comparison' to its underlying database representation

serializeComparison :: Id Comparison -> Comparison -> DB.Comparison Result
serializeComparison comparisonId' comparison = 
  DB.Comparison
    { comparisonId = comparisonId',
      concept1Id = Comparison.concept1Id comparison,
      concept2Id = Comparison.concept2Id comparison,
      concept1EloBefore = Comparison.concept1EloBefore comparison,
      concept2EloBefore = Comparison.concept2EloBefore comparison,
      concept1EloAfter = Comparison.concept1EloAfter comparison,
      concept2EloAfter = Comparison.concept2EloAfter comparison,
      winningConceptId = Comparison.winningConceptId comparison,
      comparisonModelId = Comparison.comparisonModelId comparison,
      comparisonTimestamp = Comparison.comparisonTimestamp comparison
    }

-- |
-- Transform from the database representation of a 'Comparison' to its domain representation

unserializeComparison :: DB.Comparison Result -> Comparison
unserializeComparison comparison = Comparison (concept1Id comparison) (concept2Id comparison) (concept1EloBefore comparison) (concept2EloBefore comparison) (concept1EloAfter comparison) (concept2EloAfter comparison) (winningConceptId comparison) (comparisonModelId comparison) (comparisonTimestamp comparison) 



--data Elo f = Elo
--  { eloId :: Column f (Id Domain.Elo),
--    eloConceptId :: Column f (Id Domain.Concept),
--    eloModelId :: Column f (Id Domain.Model),
--    eloScore :: Column f Int,
--    eloLastUpdate :: Column f UTCTime
--  }
--  deriving stock (Generic)
--  deriving anyclass (Rel8able)


-- ELO
-- |
-- Transform from a domain representation of a 'Elo' to its underlying database representation
--
serializeElo :: Id Elo -> Elo -> DB.Elo Result
serializeElo eloId' elo = 
  DB.Elo
    { eloId = eloId',
      eloConceptId = Elo.eloConceptId elo,
      eloModelId = Elo.eloModelId elo,
      eloScore = Elo.eloScore elo,
      eloLastUpdate = Elo.eloLastUpdate elo
    }

-- |
-- Transform from the database representation of a 'Elo' to its domain representation
unserializeElo :: DB.Elo Result -> Elo
unserializeElo elo = Elo (eloConceptId elo) (eloModelId elo) (eloScore elo) (eloLastUpdate elo)

