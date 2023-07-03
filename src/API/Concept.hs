{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Concept where

import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Concept (Concept)
import Tagger.Id (Id)
import qualified Tagger.Repository.Concept as TRC (ConceptRepository (selectAllConcepts, addConcept)) --, selectConceptByWikiLink, selectConcept 
import Data.Text (Text)
import Prelude hiding (getContents)

-- |
-- The main endpoints of the application API
--data TaggerAPI mode = TaggerAPI
--  { -- | Add a new 'Content'
--    addContent :: mode :- "add-content" :> ReqBody '[JSON] (Content Tag) :> Post '[JSON] (Id (Content Tag)),
--    -- | Retrieve all the 'User' 'Content's indexed by the provided 'Tag's
--    getContents :: mode :- "get-contents" :> QueryParams "tag" Tag :> Get '[JSON] [Owned (Content Tag)]
--  }
--  deriving stock (Generic)

-- |
-- The main endpoints of the application API
data ConceptAPI mode  = ConceptAPI
  { -- | Add a new 'Content' takes three arguments: the name of the Concept, a description of the Concept, and the WikiLink 
  -- | express in the addConcept below Text -> Text -> Text -> (Id Concept) but in the Servant way, do not user the JSON as input use three text fields
    addConcept :: mode :- "add-concept" :> Capture "concept_name" Text :> Capture "concept_description" Text :> Capture "concept_wikilink" Text :> Post '[JSON] (Id Concept), 
    -- | Retrieve all Concepts in the system, takes no query parameters returns a list of (Id Concept, Concept)
    getConcepts :: mode :- "get-concepts" :> Get '[JSON] [(Id Concept, Concept)] 
  }
  deriving stock (Generic)

-- | example usage:
-- POST http://localhost:8080/add-concept/test/test/test
--
-- GET http:    

--

conceptServer :: TRC.ConceptRepository Handler -> ConceptAPI AsServer
conceptServer conceptRepository = 
  ConceptAPI
    { addConcept = TRC.addConcept conceptRepository,
      getConcepts = TRC.selectAllConcepts conceptRepository 
    }
    
