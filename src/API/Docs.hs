{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Docs where

import API.Authentication (AuthenticationAPI)
import API.Tagger (TaggerAPI)
import API.Concept (ConceptAPI)
import API.Model (ModelAPI)
import API.Comparison (ComparisonAPI)
import API.Elo (EloAPI)
import Control.Lens ((&), (.~), (?~))
import Data.OpenApi (OpenApi, description, info, title, version)
import Data.Proxy (Proxy (Proxy))
import Servant (Get, JSON, NamedRoutes, Server, (:>))
import Servant.OpenApi (toOpenApi)

-- |
-- A single endpoint to expose the OpenAPI documentation of the application
type DocsAPI = "docs" :> Get '[JSON] OpenApi

docsServer :: Server DocsAPI
docsServer =
  return $
    toOpenApi (Proxy :: Proxy (NamedRoutes TaggerAPI)) <> toOpenApi (Proxy :: Proxy (NamedRoutes AuthenticationAPI)) 
      <> toOpenApi (Proxy :: Proxy (NamedRoutes ConceptAPI)) <> toOpenApi (Proxy :: Proxy (NamedRoutes ModelAPI)) <> toOpenApi (Proxy :: Proxy (NamedRoutes ComparisonAPI)) <> toOpenApi (Proxy :: Proxy (NamedRoutes EloAPI))
      & info . title .~ "Tagger api"
      & info . version .~ "1.0.0"
      & info . description ?~ "API endpoints for the tagger API"
