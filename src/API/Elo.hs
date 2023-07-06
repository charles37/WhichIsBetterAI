{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Elo where

import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, type (:>), Capture)
--import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Elo (Elo)
import Tagger.Concept (Concept)
import Tagger.Model (Model)
import Tagger.Id (Id (Id))
import qualified Tagger.Repository.Elo as TRC (EloRepository (getAllElos, getLeaderboard))
import Prelude hiding (getContents)
import Data.Int (Int32)

import Data.UUID (UUID)


data EloAPI mode  = EloAPI
  { 
    getElos :: mode :- "get-elos" :> Get '[JSON] [(Id Elo, Elo)] ,
    --getLeaderbpard takes a modelId and returns a list of concepts and their elos
    getLeaderboard :: mode :- "get-leaderboard" :> Capture "modelId" (UUID) :> Get '[JSON] [(Concept, Int32)]
  }
  deriving stock (Generic)

uuidToModelId :: UUID -> Id Model
uuidToModelId = Id 

eloServer :: TRC.EloRepository Handler -> EloAPI AsServer
eloServer eloRepository = 
  EloAPI
    { 
      getElos = TRC.getAllElos eloRepository,
      getLeaderboard = \modelId -> TRC.getLeaderboard eloRepository (uuidToModelId modelId)
    }
    
