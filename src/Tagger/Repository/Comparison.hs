
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Comparison where

import Tagger.Comparison (Comparison)
import Tagger.Id (Id)
import Tagger.Concept (Concept)
import Tagger.Model (Model)
import Data.Int (Int32)

-- |
-- A 'ComparisonRepository' represents a collection of 'Comparison's.
-- It is indexed by a context 'm' which wraps the results.
data ComparisonRepository m = ComparisonRepository
  {
    -- | select a comparison by 'Id'
    selectComparison :: Id Comparison -> m Comparison,
    
    selectAllComparisons :: m [(Id Comparison, Comparison)],
    -- | adds a 'Comparison'

    --addComparison :: Id Concept -> Id Concept -> Int -> Int -> Int -> Int -> Id Model -> m (Id Comparison),

    doComparisonSingle :: Id Model -> Id Concept -> Id Concept -> m (Id Comparison, Comparison),

    doComparisonAllModels :: Id Concept -> Id Concept -> m [(Id Comparison, Comparison)],

    runRandomComparisons :: Int32 -> m [(Id Comparison, Comparison)]
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'ComparisonRepository' is operating
hoist :: (forall a. m a -> n a) -> ComparisonRepository m -> ComparisonRepository n
hoist f ComparisonRepository {selectComparison, selectAllComparisons, doComparisonSingle, doComparisonAllModels, runRandomComparisons} =
    ComparisonRepository (f . selectComparison) (f selectAllComparisons) (((f .) .) . doComparisonSingle) ((f .) . doComparisonAllModels) (f . runRandomComparisons)
