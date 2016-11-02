-- | Helper functions to work with JSON.
module Dashboard.Server.Routes.Json where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty (encodePretty)

import Network.HTTP.Types (hContentType, ok200, badRequest400)
import Network.Wai (Response, Request, responseLBS, lazyRequestBody)

import Dashboard.Server.Monad (ServerT, liftIO)

-- | Jsonify a 'Route' function.
jsonify :: ToJSON b => (a -> ServerT IO b) -> (a -> ServerT IO Response)
jsonify f x = responseLBS ok200 hs . encodePretty <$> f x
  where hs = [(hContentType, "application/json")]

-- | Objectify a 'Route' function
objectify :: FromJSON a => (a -> ServerT IO Response) -> (Request -> ServerT IO Response)
objectify f req = do
    o <- liftIO $ decode <$> lazyRequestBody req
    case o of Nothing -> return $ responseLBS badRequest400 hs "Bad JSON\n"
              Just x -> f x
  where
    hs = [(hContentType, "text/plain")]
