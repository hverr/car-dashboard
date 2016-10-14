-- | Helper functions to work with JSON.
module Dashboard.Server.Routes.Json where

import Data.Aeson (ToJSON, encode)

import Network.HTTP.Types (hContentType, ok200)
import Network.Wai (Response, responseLBS)

import Dashboard.Server.Monad (ServerT)

-- | Jsonify a 'Route' function.
jsonify :: ToJSON b => (a -> ServerT IO b) -> (a -> ServerT IO Response)
jsonify f x = responseLBS ok200 hs . encode <$> f x
  where hs = [(hContentType, "application/json")]
