{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Elo where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Data.OpenApi (ToSchema)

import Tagger.Id (Id)
import Tagger.Concept (Concept)
import Tagger.Model (Model)

import Data.Time.Clock (UTCTime)

import Data.Int (Int32)


data Elo = Elo
  { --  eloId :: UUID
    eloConceptId :: Id Concept,
    eloModelId :: Id Model,
    eloScore :: Int32,
    eloLastUpdate :: UTCTime
    
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Elo


instance ToSchema Elo
