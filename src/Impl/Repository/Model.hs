
module Impl.Repository.Model (inMemory, postgres) where 

import Control.Monad.Trans.Except (ExceptT)
import Impl.Repository.Model.Error (ModelRepositoryError)
import qualified Impl.Repository.Model.InMemory as IM
import qualified Impl.Repository.Model.Postgres as PG
import qualified Infrastructure.Database as DB
import Tagger.Repository.Model (ModelRepository (..))

postgres :: DB.Handle -> ModelRepository (ExceptT ModelRepositoryError IO)
postgres = PG.repository

inMemory :: IM.Table -> ModelRepository (ExceptT ModelRepositoryError IO)
inMemory = IM.repository


