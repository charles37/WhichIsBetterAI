{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Model where

import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Model (Model)
import Tagger.Id (Id)
import qualified Tagger.Repository.Model as TRM (ModelRepository (selectAllModels, addModel)) 
import Data.Text (Text)
import Prelude hiding (getContents)

-- |
-- The main endpoints of the application API
data ModelAPI mode  = ModelAPI
  { -- | Add a new 'Content' takes three arguments: the name of the Model, a description of the Model, and the WikiLink 
  -- | express in the addModel below Text -> Text -> Text -> (Id Model) but in the Servant way, do not user the JSON as input use three text fields
    addModel :: mode :- "add-model" :> Capture "model_name" Text :> Capture "model_description" Text :> Post '[JSON] (Id Model), 
    -- | Retrieve all Models in the system, takes no query parameters returns a list of (Id Model, Model)
    getModels :: mode :- "get-models" :> Get '[JSON] [(Id Model, Model)] 

    -- | Retrieve a single model by ID
  }
  deriving stock (Generic)

-- | example usage:
-- POST http://localhost:8080/add-model/test/test/



modelServer :: TRM.ModelRepository Handler -> ModelAPI AsServer
modelServer modelRepository = 
  ModelAPI
    { addModel = TRM.addModel modelRepository,
      getModels = TRM.selectAllModels modelRepository 
    }
 
