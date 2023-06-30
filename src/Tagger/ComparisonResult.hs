{-# LANGUAGE DeriveGeneric #-}tagger-user

module Tagger.ComparisonResult where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data ComparisonResult = ComparisonResult
  { comparisonId :: Int,
    concept1Id :: Int,
    concept2Id :: Int,
    winningConceptId :: Int,
    modelId :: Int,
    comparisonTimestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ComparisonResult

