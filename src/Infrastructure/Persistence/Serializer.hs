module Infrastructure.Persistence.Serializer where

import Infrastructure.Persistence.Schema (contentContent, contentId, contentUserId, tagId, tagName, userId, userName, userPassword, conceptId, conceptName, conceptDescription, conceptWikiLink)
import qualified Infrastructure.Persistence.Schema as DB (Content (Content), Tag (Tag), User (User), Concept (Concept))
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

