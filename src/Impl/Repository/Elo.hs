module Impl.Repository.Elo (postgres) where 
--module Impl.Repository.Elo (inMemory, postgres) where 

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.Elo.Error (EloRepositoryError)
--import qualified Impl.Repository.Elo.InMemory as IM
import qualified Impl.Repository.Elo.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.Elo (EloRepository (..))

postgres :: DB.Handle -> EloRepository (ExceptT EloRepositoryError IO)
postgres = PG.repository


-- BEN TODO: I decided to do the postgres implementation first, InMemory Soon (tm) 
--inMemory :: IM.Table -> EloRepository (ExceptT EloRepositoryError IO)
--inMemory = IM.repository


