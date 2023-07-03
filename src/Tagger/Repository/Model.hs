
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Model where

import Tagger.Model (Model)
import Tagger.Id (Id)
import Data.Text (Text)

-- |
-- A 'ModelRepository' represents a collection of 'Model's.
-- It is indexed by a context 'm' which wraps the results.
data ModelRepository m = ModelRepository
  { -- | selects a 'Model' by its 'Id'
    selectModel :: Id Model -> m Model,
    -- | selects a 'Model' by its 'WikiLink' 
    getModelByName :: Text -> m (Id Model, Model),
    -- | selects all the 'Model's
    selectAllModels :: m [(Id Model, Model)],
    -- | adds a 'Model'
    addModel :: Text -> Text -> m (Id Model)
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ModelRepository' is operating
hoist :: (forall a. m a -> n a) -> ModelRepository m -> ModelRepository n
hoist f ModelRepository {selectModel, getModelByName, selectAllModels, addModel} =
    ModelRepository (f . selectModel) (f . getModelByName) (f selectAllModels) ((f .) . addModel) 

