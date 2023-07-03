{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Concept where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.OpenApi (ToSchema)

data Concept = Concept
  { --  conceptId :: UUID
    conceptName :: Text,
    conceptDescription :: Text,
    conceptWikiLink :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Concept

instance ToSchema Concept
