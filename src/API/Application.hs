{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API.Application where

import API.AppServices (AppServices (..))
import API.Authentication (AuthenticationAPI, authenticationServer)
import API.Docs (DocsAPI, docsServer)
import API.Healthcheck (HealthcheckAPI, healthcheckServer)
import API.Tagger (TaggerAPI, taggerServer)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant (Context (EmptyContext, (:.)), Handler, err401, serveWithContext)
import Servant.API (NamedRoutes, type (:>))
import Servant.API.Generic ((:-))
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll), defaultCookieSettings)
import Servant.Server.Generic (AsServer)
import Tagger.Id (Id)
import Tagger.User (User)
import Tagger.Repository.Content (ContentRepository)

import API.Concept (ConceptAPI, conceptServer)
import API.Model (ModelAPI, modelServer)
import API.Elo (EloAPI, eloServer)
import API.Comparison (ComparisonAPI, comparisonServer)

type API = NamedRoutes ApplicationAPI

-- |
-- Collects all the API groups exposed by the application
data ApplicationAPI mode = ApplicationAPI
  { tagger :: mode :- Auth '[JWT] (Id User) :> NamedRoutes TaggerAPI,
    docs :: mode :- DocsAPI,
    healthcheck :: mode :- HealthcheckAPI,
    authentication :: mode :- NamedRoutes AuthenticationAPI,
    concepts :: mode :- NamedRoutes ConceptAPI,
    models :: mode :- NamedRoutes ModelAPI,
    elos :: mode :- NamedRoutes EloAPI,
    comparisons :: mode :- NamedRoutes ComparisonAPI
  }
  deriving stock (Generic)

-- |
-- For the endpoints which actually require authentication, checks whether the request provides a valid authentication token.
-- Otherwise it returns a 401 response
authenticatedTaggerServer :: ContentRepository Handler -> AuthResult (Id User) -> TaggerAPI AsServer
authenticatedTaggerServer contentRepository = \case
  (Authenticated userId) -> taggerServer userId contentRepository
  _ -> throwAll err401

-- |
-- Setup all the application server, providing the services needed by the various endpoints
server :: AppServices -> ApplicationAPI AsServer
server AppServices {passwordManager, contentRepository, userRepository, authenticateUser, conceptRepository, modelRepository, eloRepository, comparisonRepository} =
  ApplicationAPI
    { tagger = authenticatedTaggerServer contentRepository,
      docs = docsServer,
      healthcheck = healthcheckServer,
      authentication = authenticationServer passwordManager authenticateUser userRepository,
      concepts = conceptServer conceptRepository,
      models = modelServer modelRepository,
      elos = eloServer eloRepository,
      comparisons = comparisonServer comparisonRepository
    }

app :: AppServices -> Application
app appServices =
  serveWithContext
    (Proxy :: Proxy API)
    (defaultCookieSettings :. jwtSettings appServices :. EmptyContext)
    (server appServices)
