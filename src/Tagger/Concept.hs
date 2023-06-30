{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Concept where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Concept = Concept
  { conceptId :: Int,
    conceptName :: Text,
    conceptDescription :: Text,
    conceptWikiLink :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Concept

