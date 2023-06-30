module Impl.Repository.Concept (inMemory, postgres) where 

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.Concept.Error (ConceptRepositoryError)
import qualified Impl.Repository.Concept.InMemory as IM
import qualified Impl.Repository.Concept.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.Concept (ConceptRepository (..))

postgres :: DB.Handle -> ConceptRepository (ExceptT ConceptRepositoryError IO)
postgres = PG.repository

inMemory :: IM.Table -> ConceptRepository (ExceptT ConceptRepositoryError IO)
inMemory = IM.repository



