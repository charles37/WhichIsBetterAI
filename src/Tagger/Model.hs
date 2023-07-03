{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Tagger.Model where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.OpenApi (ToSchema)

data Model = Model
  { --  modeLID :: UUID
    modelName :: Text,
    modelDescription :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Model

instance ToSchema Model
