{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Comparison where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Data.OpenApi (ToSchema)

import Tagger.Id (Id)
import Tagger.Concept (Concept)
import Tagger.Model (Model)

import Data.Time (UTCTime)

import Data.Int (Int32)

data Comparison = Comparison
  { --  comparisonID :: UUID
    concept1Id :: Id Concept,
    concept2Id :: Id Concept,
    concept1EloBefore :: Int32,
    concept2EloBefore :: Int32,
    concept1EloAfter :: Int32,
    concept2EloAfter :: Int32,
    winningConceptId :: Id Concept,
    comparisonModelId :: Id Model,
    comparisonTimestamp :: UTCTime 
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Comparison

instance ToSchema Comparison
