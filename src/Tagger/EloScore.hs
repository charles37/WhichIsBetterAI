{-# LANGUAGE DeriveGeneric #-}

module Tagger.EloScore where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data EloScore = EloScore
  { eloId :: Int,
    conceptId :: Int,
    modelId :: Int,
    eloScore :: Int,
    lastUpdateTimestamp :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON EloScore

