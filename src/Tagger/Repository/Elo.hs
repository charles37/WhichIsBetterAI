
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Tagger.Repository.Elo where

import Tagger.Elo (Elo)
import Tagger.Id (Id)
import Tagger.Concept (Concept)
import Tagger.Model (Model)

import Data.Int (Int32)

-- |
-- A 'EloRepository' represents a collection of 'Elo's.
-- It is indexed by a context 'm' which wraps the results.
data EloRepository m = EloRepository
  {
    -- | selects a 'Elo' by its 'WikiLink' 
    getEloScoreByConceptAndModel :: Id Model -> Id Concept -> m Int32,

    -- | get the leaderboard for a Model
    getElosByModel :: Id Model -> m [Elo],

    -- | see the difference in Elos for a particular Model
    getElosByConcept :: Id Concept -> m [Elo],

    -- | selects all the 'Elo's
    getAllElos :: m [(Id Elo, Elo)],

    getLeaderboard :: Id Model -> m [(Concept, Int32)]


    
  }

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'EloRepository' is operating
hoist :: (forall a. m a -> n a) -> EloRepository m -> EloRepository n
hoist f EloRepository {getEloScoreByConceptAndModel, getElosByModel, getElosByConcept, getAllElos, getLeaderboard} =
    EloRepository  ((f .) . getEloScoreByConceptAndModel) ((f .) getElosByModel) ((f .) getElosByConcept) (f getAllElos) ((f .) getLeaderboard)

