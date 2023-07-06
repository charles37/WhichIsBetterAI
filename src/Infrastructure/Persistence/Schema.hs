{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Infrastructure.Persistence.Schema where

import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (Column, Expr, Name, Rel8able, Result, TableSchema (..), lit)
import qualified Tagger.Content as Domain (Content)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import qualified Tagger.Tag as Domain (Tag)
import qualified Tagger.User as Domain (User)
import qualified Tagger.Concept as Domain (Concept)
import qualified Tagger.Model as Domain (Model)
import qualified Tagger.Comparison as Domain (Comparison)
import qualified Tagger.Elo as Domain (Elo)



import Data.Int (Int32)
   
import Data.Time.Clock (UTCTime)
-- TAG

-- |
-- The database representation of a 'Tag'
data Tag f = Tag
  { tagId :: Column f (Id Domain.Tag),
    tagName :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Tag' table
tagSchema :: TableSchema (Tag Name)
tagSchema =
  TableSchema
    { name = "tags",
      schema = Nothing,
      columns =
        Tag
          { tagId = "id",
            tagName = "name"
          }
    }

-- |
-- Allows to lift a 'Tag' with no context into the 'Expr' context
litTag :: Tag Result -> Tag Expr
litTag (Tag id' name') = Tag (lit id') (lit name')

-- CONTENT

-- |
-- The database representation of a 'Content'
data Content f = Content
  { contentId :: Column f (Id (Domain.Content Domain.Tag)),
    contentContent :: Column f Text,
    contentUserId :: Column f (Id Domain.User)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'Content' table
contentSchema :: TableSchema (Content Name)
contentSchema =
  TableSchema
    { name = "contents",
      schema = Nothing,
      columns =
        Content
          { contentId = "id",
            contentContent = "content",
            contentUserId = "user_id"
          }
    }

-- |
-- Allows to lift a 'Content' with no context into the 'Expr' context
litContent :: Content Result -> Content Expr
litContent (Content id' content' userId') = Content (lit id') (lit content') (lit userId')

-- CONTENTS_TAGS

-- |
-- The database representation of a connection between a 'Content' and a 'Tag'
data ContentsTags f = ContentsTags
  { ctContentId :: Column f (Id (Domain.Content Domain.Tag)),
    ctTagId :: Column f (Id Domain.Tag)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'ContentsTags' table
contentsTagsSchema :: TableSchema (ContentsTags Name)
contentsTagsSchema =
  TableSchema
    { name = "contents_tags",
      schema = Nothing,
      columns =
        ContentsTags
          { ctContentId = "content_id",
            ctTagId = "tag_id"
          }
    }

-- USERS

-- |
-- The database representation of a 'User'
data User f = User
  { userId :: Column f (Id Domain.User),
    userName :: Column f Text,
    userPassword :: Column f EncryptedPassword
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'User' table
userSchema :: TableSchema (User Name)
userSchema =
  TableSchema
    { name = "users",
      schema = Nothing,
      columns =
        User
          { userId = "id",
            userName = "name",
            userPassword = "password"
          }
    }

-- |
-- Allows to lift a 'User' with no context into the 'Expr' context
litUser :: User Result -> User Expr
litUser (User id' name' password) = User (lit id') (lit name') (lit password)

-- | Concepts
--data Concept = Concept
--  { --  conceptId :: UUID
--    conceptName :: Text,
--    conceptDescription :: Text,
--    conceptWikiLink :: Text
--  }
--  deriving stock (Eq, Show, Generic)

data Concept f = Concept
  { conceptId :: Column f (Id Domain.Concept),
    conceptName :: Column f Text,
    conceptDescription :: Column f Text,
    conceptWikiLink :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

conceptSchema :: TableSchema (Concept Name)
conceptSchema =
  TableSchema
    { name = "concepts",
      schema = Nothing,
      columns =
        Concept
          { conceptId = "concept_id",
            conceptName = "concept_name",
            conceptDescription = "concept_description",
            conceptWikiLink = "concept_wiki_link"
          }
    }

litConcept :: Concept Result -> Concept Expr
litConcept (Concept id' name' description' wikiLink') =
  Concept (lit id') (lit name') (lit description') (lit wikiLink')

--data Model = Model
--  { --  modelID :: UUID
--    modelName :: Text,
--    modelDescription :: Text,
--  }
--  deriving stock (Eq, Show, Generic)
--

data Model f = Model
  { modelId :: Column f (Id Domain.Model),
    modelName :: Column f Text,
    modelDescription :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

modelSchema :: TableSchema (Model Name)
modelSchema =
  TableSchema
    { name = "ai_models",
      schema = Nothing,
      columns =
        Model
          { modelId = "model_id",
            modelName = "model_name",
            modelDescription = "model_description"
          }
    }

litModel :: Model Result -> Model Expr
litModel (Model id' name' description') =
  Model (lit id') (lit name') (lit description')


--data Comparison = Comparison
--  { --  comparisonID :: UUID
--    concept1Id :: Id Concept,
--    concept2Id :: Id Concept,
--    concept1EloBefore :: Int,
--    concept2EloBefore :: Int,
--    winning_conceptId :: Id Concept,
--    modelId :: Id Model,
--    comparisonTimestamp :: UTCTime 
--  }
--  deriving stock (Eq, Show, Generic)
--

data Comparison f = Comparison
  { comparisonId :: Column f (Id Domain.Comparison),
    concept1Id :: Column f (Id Domain.Concept),
    concept2Id :: Column f (Id Domain.Concept),
    concept1EloBefore :: Column f Int32,
    concept2EloBefore :: Column f Int32,
    concept1EloAfter :: Column f Int32,
    concept2EloAfter :: Column f Int32,
    winningConceptId :: Column f (Id Domain.Concept),
    comparisonModelId :: Column f (Id Domain.Model),
    comparisonTimestamp :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

comparisonSchema :: TableSchema (Comparison Name)
comparisonSchema =
  TableSchema
    { name = "comparison_results" ,
      schema = Nothing,
      columns =
        Comparison
          { comparisonId = "comparison_id",
            concept1Id = "concept1_id",
            concept2Id = "concept2_id",
            concept1EloBefore = "concept1_elo_before",
            concept2EloBefore = "concept2_elo_before",
            concept1EloAfter  = "concept1_elo_after",
            concept2EloAfter  = "concept2_elo_after",
            winningConceptId = "winning_concept_id",
            comparisonModelId = "model_id",
            comparisonTimestamp = "comparison_timestamp"
          }
    }

litComparison :: Comparison Result -> Comparison Expr
litComparison (Comparison id' concept1Id' concept2Id' concept1EloBefore' concept2EloBefore' concept1EloAfter' concept2EloAfter' winningConceptId' modelId' timestamp') =
  Comparison (lit id') (lit concept1Id') (lit concept2Id') (lit concept1EloBefore') (lit concept2EloBefore') (lit concept1EloAfter') (lit concept2EloAfter') (lit winningConceptId') (lit modelId') (lit timestamp')


--data Elo = Elo
--  { --  eloId :: UUID
--    conceptId :: Id Concept,
--    modelId :: Id Model,
--    eloScore :: Int,
--    lastUpdate :: UTCTime
--    
--  }
--  deriving stock (Eq, Show, Generic)

data Elo f = Elo
  { eloId :: Column f (Id Domain.Elo),
    eloConceptId :: Column f (Id Domain.Concept),
    eloModelId :: Column f (Id Domain.Model),
    eloScore :: Column f Int32,
    eloLastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)


eloSchema :: TableSchema (Elo Name)
eloSchema =
  TableSchema
    { name = "elo_scores",
      schema = Nothing,
      columns =
        Elo
            { eloId = "elo_id",
                eloConceptId = "concept_id",
                eloModelId = "model_id",
                eloScore = "elo_score",
                eloLastUpdate = "last_update_timestamp"
            }
    }

litElo :: Elo Result -> Elo Expr
litElo (Elo id' conceptId' modelId' eloScore' lastUpdate') =
  Elo (lit id') (lit conceptId') (lit modelId') (lit eloScore') (lit lastUpdate')
