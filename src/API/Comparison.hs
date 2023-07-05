{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Comparison where

import GHC.Generics (Generic)
import Servant (Handler)
--import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API (Get, JSON, type (:>))
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Comparison (Comparison)
import Tagger.Id (Id)
import qualified Tagger.Repository.Comparison as TRC (ComparisonRepository (selectAllComparisons)) --, 
import Prelude hiding (getContents)



data ComparisonAPI mode  = ComparisonAPI
  { 
    getComparisons :: mode :- "get-comparisons" :> Get '[JSON] [(Id Comparison, Comparison)] 
  }
  deriving stock (Generic)



comparisonServer :: TRC.ComparisonRepository Handler -> ComparisonAPI AsServer
comparisonServer comparisonRepository = 
  ComparisonAPI
    { 
      getComparisons = TRC.selectAllComparisons comparisonRepository 
    }
 
