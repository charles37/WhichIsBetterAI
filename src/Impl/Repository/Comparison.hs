module Impl.Repository.Comparison (postgres) where 
--module Impl.Repository.Comparison (inMemory, postgres) where 

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.Comparison.Error (ComparisonRepositoryError)
--import qualified Impl.Repository.Comparison.InMemory as IM
import qualified Impl.Repository.Comparison.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.Comparison (ComparisonRepository (..))

postgres :: DB.Handle -> ComparisonRepository (ExceptT ComparisonRepositoryError IO)
postgres = PG.repository

--inMemory :: IM.Table -> ComparisonRepository (ExceptT ComparisonRepositoryError IO)
--inMemory = IM.repository



