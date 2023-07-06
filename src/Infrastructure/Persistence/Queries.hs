{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infrastructure.Persistence.Queries where

import qualified Data.List as List (filter)
import Data.Text (Text)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction (statement)
import Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Write), transaction)
import Infrastructure.Persistence.Schema ( Content (..), ContentsTags (..),
    Tag (..), User (userName), contentSchema, contentsTagsSchema, litContent, litTag, tagSchema, 
    userId, userSchema, 
    conceptSchema, modelSchema, comparisonSchema, eloSchema,
    Concept(..), Model(..), Comparison(..), Elo(..) 
    )
import Rel8 (Expr, Insert (..), Name, OnConflict (..), Update (..), Query, Rel8able, Result, TableSchema,
    each, filter, in_, insert, lit, many, select, values, where_, update, (==.), (&&.))
import Tagger.Id (Id)
import qualified Tagger.User as Domain (User)
import qualified Tagger.Concept as Domain (Concept) 
import qualified Tagger.Model as Domain (Model)
import qualified Tagger.Comparison as Domain (Comparison)
--import qualified Tagger.Elo as Domain (Elo)
import Data.Maybe (listToMaybe)
import Prelude hiding (filter)
--import Data.Functor.Contravariant ((>$<))

import Data.Int (Int32)

-- SELECT CONTENTS

-- |
-- Selects the 'ContentsTags' for a given 'Content'
contentsTagsForContent :: Content Expr -> Query (ContentsTags Expr)
contentsTagsForContent content =
  each contentsTagsSchema
    >>= filter
      ( \contentTag' ->
          ctContentId contentTag' ==. contentId content
      )

-- |
-- Selects the 'Tags' associated with a given 'Content'
tagsForContent :: Content Expr -> Query (Tag Expr)
tagsForContent content = do
  tag <- each tagSchema
  contentTag' <- contentsTagsForContent content
  where_ $ tagId tag ==. ctTagId contentTag'
  return tag

-- |
-- Selects the 'User' who ownes a 'Content'
userForContent :: Content Expr -> Query (User Expr)
userForContent content =
  each userSchema
    >>= filter
      ( \user ->
          userId user ==. contentUserId content
      )

-- |
-- Given a 'Domain.User' 'Id', retrieves all the contents for that specific user
selectUserContents :: Id Domain.User -> Session [(Content Result, [Tag Result], User Result)]
selectUserContents userId' = statement () . select $ do
  -- Select all content for the given user
  content <-
    each contentSchema
      >>= filter
        ( \content ->
            contentUserId content ==. lit userId'
        )
  -- Select tags for each content
  tags <- many $ tagsForContent content
  -- Select user for each content
  user <- userForContent content
  return (content, tags, user)

-- SELECT TAGS

-- |
-- Selects all tags present in the database among the requested ones
selectTags :: [Tag Result] -> Statement () [Tag Result]
selectTags tagNames = select $ each tagSchema >>= filter ((`in_` (tagName . litTag <$> tagNames)) . tagName)

-- ADD CONTENT

-- |
-- Adds a number of rows to the specified 'TableSchema'
add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Statement () ()
add schema rows' =
  insert $
    Insert
      { into = schema,
        rows = values rows',
        onConflict = Abort,
        returning = pure ()
      }

-- |
-- Creates a 'ContentTag' given a 'Content' and a 'Tag'
contentTag :: Content f -> Tag f -> ContentsTags f
contentTag content tag =
  ContentsTags
    { ctContentId = contentId content,
      ctTagId = tagId tag
    }

-- |
-- Removes the 'alreadyPresentTags' from 'allTags'
removeAlreadyPresentTags :: [Tag Result] -> [Tag Result] -> [Tag Result]
removeAlreadyPresentTags allTags alreadyPresentTags = List.filter (\tag -> tagName tag `notElem` (tagName <$> alreadyPresentTags)) allTags

-- |
-- Given a 'Content' and a list of 'Tag's, it inserts the new content into the database associating to it the provided tags.
-- To avoid 'Tag' repetitions, it goes through the following steps:
--
-- * selects 'Tag's from the database
-- * replaces the generated 'UUID's with the one coming from the database
-- * inserts the new 'Tag's
-- * inserts the 'Content'
-- * inserts the 'ContentsTags' to link the 'Content' with its 'Tags'
addContentWithTags :: Content Result -> [Tag Result] -> Session ()
addContentWithTags content tags = transaction Serializable Write $ do
  alreadyPresentTags <- Transaction.statement () (selectTags tags)
  let newTags = litTag <$> removeAlreadyPresentTags tags alreadyPresentTags
  Transaction.statement () $ add tagSchema newTags
  Transaction.statement () $ add contentSchema [litContent content]
  Transaction.statement () $ add contentsTagsSchema (contentTag (litContent content) <$> (litTag <$> alreadyPresentTags) <> newTags)

-- SELECT USER BY USERNAME

-- |
-- Describes the possible error cases for queries that expect exactly one row as a result.
data WrongNumberOfResults
  = NoResults
  | MoreThanOneResult
  deriving (Show)

-- |
-- Given a list of results, succeed if there is only one in the list, otherwise fail with the appropriate error message
justOne :: [a Result] -> Either WrongNumberOfResults (a Result)
justOne = \case
  [] -> Left NoResults
  [a] -> Right a
  _ -> Left MoreThanOneResult

-- |
-- Retrieve from the database a user with the provided name.
-- If in the database we find none or more the one, it returns the appropriate error message
selectUserByName :: Text -> Session (Either WrongNumberOfResults (User Result))
selectUserByName name = statement () query
  where
    query = fmap justOne . select $ do
      users <- each userSchema
      filter (\user -> userName user ==. lit name) users

-- ADD USER

-- |
-- Add a new 'User' in the database
addUser :: User Expr -> Session ()
addUser = statement () . add userSchema . pure


-- SELECT CONCEPT

-- |
-- A 'ConceptRepository' represents a collection of 'Concept's.
-- It is indexed by a context 'm' which wraps the results.
--data ConceptRepository m = ConceptRepository
--  { -- | selects a 'Concept' by its 'Id'
--    selectConcept :: Id Concept -> m Concept,
--    -- | selects a 'Concept' by its 'Name' (it can possibly return more than one 'Concept')
--    selectConceptByName :: Text -> m [Concept], 
--    -- | selects all the 'Concept's
--    selectAllConcepts :: m [Concept],
--    -- | adds a 'Concept'
--    addConcept :: Concept -> m (Id Concept)
--  }
--

--data Concept = Concept
--  { --  conceptId :: UUID
--    conceptName :: Text,
--    conceptDescription :: Text,
--    conceptWikiLink :: Text
--  }
--  deriving stock (Eq, Show, Generic)

addConcept :: Concept Expr -> Session ()
addConcept = statement () . add conceptSchema . pure

selectConcept :: (Id Domain.Concept) -> Session (Maybe (Concept Result))
selectConcept conceptId' = statement () query
  where
    query = fmap listToMaybe . select $ do
      concepts <- each conceptSchema
      filter (\concept -> conceptId concept ==. lit conceptId') concepts

selectConceptByWikiLink :: Text -> Session (Either WrongNumberOfResults (Concept Result)) 
selectConceptByWikiLink wikiLink = statement () query
  where
    query = fmap justOne . select $ do
      concepts <- each conceptSchema
      filter (\concept -> conceptWikiLink concept ==. lit wikiLink) concepts

selectAllConcepts :: Session [Concept Result] 
selectAllConcepts = statement () query
  where
    query = select $ each conceptSchema

-- |
-- A 'ModelRepository' represents a collection of 'Model's.
-- It is indexed by a context 'm' which wraps the results.
--data ModelRepository m = ModelRepository
--  { -- | selects a 'Model' by its 'Id'
--    selectModel :: Id Model -> m Model,
--    -- | selects a 'Model' by its 'WikiLink' 
--    getModelByName :: Text -> m (Id Model, Model),
--    -- | selects all the 'Model's
--    selectAllModels :: m [(Id Model, Model)],
--    -- | adds a 'Model'
--    addModel :: Text -> Text -> m (Id Model)
--  }

--data Model = Model
--  { --  modelId :: UUID
--  modelName :: Text,
--  modelDescription :: Text,
--  }

addModel :: Model Expr -> Session ()
addModel = statement () . add modelSchema . pure

selectModel :: (Id Domain.Model) -> Session (Maybe (Model Result))
selectModel modelId' = statement () query
  where
    query = fmap listToMaybe . select $ do
      models <- each modelSchema
      filter (\model -> modelId model ==. lit modelId') models

selectModelByName :: Text -> Session (Either WrongNumberOfResults (Model Result))
selectModelByName name = statement () query
  where
    query = fmap justOne . select $ do
      models <- each modelSchema
      filter (\model -> modelName model ==. lit name) models

selectAllModels :: Session [Model Result]
selectAllModels = statement () query
  where
    query = select $ do
      models <- each modelSchema
      pure models
    

--data Comparison = Comparison
--  { --  comparisonID :: UUID
--    concept1Id :: Id Concept,
--    concept2Id :: Id Concept,
--    concept1EloBefore :: Int,
--    concept2EloBefore :: Int,
--    concept1EloAfter :: Int,
--    concept2EloAfter :: Int,
--    winning_conceptId :: Id Concept,
--    modelId :: Id Model,
--    comparisonTimestamp :: UTCTime 
--  }
--  deriving stock (Eq, Show, Generic)

--data ComparisonRepository m = ComparisonRepository
--  {
--    doComparison :: Id Model -> Id Concept -> Id Concept -> m Comparison,
--
--    -- | select a comparison by 'Id'
--    selectComparison :: Id Comparison -> m Comparison,
--    
--    selectAllComparisons :: m [(Id Comparison, Comparison)],
--
--    addComparison :: Comparison -> m (Id Comparison)
--  }

selectComparison :: (Id Domain.Comparison) -> Session (Maybe (Comparison Result))
selectComparison comparisonId' = statement () query
  where
    query = fmap listToMaybe . select $ do
      comparisons <- each comparisonSchema
      filter (\comparison -> comparisonId comparison ==. lit comparisonId') comparisons

selectAllComparisons :: Session [Comparison Result]
selectAllComparisons = statement () query
  where
    query = select $ do
      comparisons <- each comparisonSchema
      pure comparisons


addComparison :: Comparison Expr -> Session ()
addComparison = statement () . add comparisonSchema . pure


--data Elo f = Elo
--  { eloId :: Column f (Id Domain.Elo),
--    eloConceptId :: Column f (Id Domain.Concept),
--    eloModelId :: Column f (Id Domain.Model),
--    eloScore :: Column f Int,
--    eloLastUpdate :: Column f UTCTime
--  }
--  deriving stock (Generic)
--  deriving anyclass (Rel8able)


-- |
-- Gets elo_score from the elo table by searching by conceptId and modelId, should only return one result

selectElosByConceptAndModel :: (Id Domain.Model) -> (Id Domain.Concept) -> Session (Maybe (Elo Result))
selectElosByConceptAndModel modelId' conceptId' = statement () query
  where
    query = fmap listToMaybe . select $ do
      elos <- each eloSchema
      filter (\elo -> eloConceptId elo ==. lit conceptId' &&. eloModelId elo ==. lit modelId') elos 

selectElosByConcept :: (Id Domain.Concept) -> Session [Elo Result]
selectElosByConcept conceptId' = statement () query
  where
    query = select $ do
      elos <- each eloSchema
      filter (\elo -> eloConceptId elo ==. lit conceptId') elos

selectElosByModel :: (Id Domain.Model) -> Session [Elo Result]
selectElosByModel modelId' = statement () query
  where
    query = select $ do
      elos <- each eloSchema
      filter (\elo -> eloModelId elo ==. lit modelId') elos


     

selectAllElos :: Session [Elo Result]
selectAllElos = statement () query
  where
    query = select $ each eloSchema

addElo :: Elo Expr -> Session ()
addElo = statement () . add eloSchema . pure

updateScore :: (Id Domain.Model) -> (Id Domain.Concept) -> Int32 -> Session ()
updateScore modelId' conceptId' score = statement () query
  where
    query = update $
        Update {
            target = eloSchema,
            from = pure (),
            set = \_ row -> row { eloScore = lit score },
            updateWhere = \_ row -> eloModelId row ==. lit modelId' &&. eloConceptId row ==. lit conceptId',
            returning = pure ()
            }
