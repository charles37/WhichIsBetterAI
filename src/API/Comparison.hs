{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Comparison where

import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Comparison (Comparison)
import Tagger.Id (Id)
import qualified Tagger.Repository.Comparison as TRC (ComparisonRepository (selectAllComparisons, runRandomComparisons)) --, 
import Prelude hiding (getContents)

import Data.Int (Int32)


data ComparisonAPI mode  = ComparisonAPI
  { 
    getComparisons :: mode :- "get-comparisons" :> Get '[JSON] [(Id Comparison, Comparison)],
    --runRandomComparisons takes in an Int32 and returns a list of comparisons, it also modifies the database so it should be a POST
    runRandomComparisons :: mode :- "run-random-comparisons" :> Capture "number" Int32 :> Post '[JSON] [(Id Comparison, Comparison)] 
  }
  deriving stock (Generic)


comparisonServer :: TRC.ComparisonRepository Handler -> ComparisonAPI AsServer
comparisonServer comparisonRepository = 
  ComparisonAPI
    { 
      getComparisons = TRC.selectAllComparisons comparisonRepository ,
      runRandomComparisons = TRC.runRandomComparisons comparisonRepository 
    }
 
