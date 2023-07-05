{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module API.Elo where

import GHC.Generics (Generic)
import Servant (Handler)
import Servant.API (Get, JSON, type (:>))
--import Servant.API (Get, JSON, Post, type (:>), Capture)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServer)
import Tagger.Elo (Elo)
import Tagger.Id (Id)
import qualified Tagger.Repository.Elo as TRC (EloRepository (getAllElos)) --, 
import Prelude hiding (getContents)


--  EloRepository
--    { getEloScoreByConceptAndModel = postgresGetEloScoreByConceptAndModel handle,
--      getElosByModel =  postgresGetElosByModel handle,
--      getElosByConcept = postgresGetElosByConcept handle,
--      getAllElos = postgresGetAllElos handle
--    }


data EloAPI mode  = EloAPI
  { 
    getElos :: mode :- "get-elos" :> Get '[JSON] [(Id Elo, Elo)] 
  }
  deriving stock (Generic)



eloServer :: TRC.EloRepository Handler -> EloAPI AsServer
eloServer eloRepository = 
  EloAPI
    { 
      getElos = TRC.getAllElos eloRepository 
    }
    
