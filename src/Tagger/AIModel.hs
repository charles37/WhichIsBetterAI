{-# LANGUAGE DeriveGeneric #-}

module Tagger.AIModel where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data AIModel = AIModel
  { modelId :: Int,
    modelName :: Text,
    modelDescription :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AIModel

