-- | This module contains the HTTP routes
module Dashboard.Server.Routes where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Network.Wai (Request, Response, responseLBS)
import Network.HTTP.Types (hContentType, ok200)

import Dashboard.Server.Monad (ServerT)

-- | A generic route is a predicate and a response generating function.
data Route = Route (Request -> Bool) (Request -> ServerT IO Response)

-- | Match a route from a list of installed routes.
match :: [Route] -> Request -> Maybe (Request -> ServerT IO Response)
match [] _ = Nothing
match (Route pred' f:xs) req = if pred' req then Just f else match xs req

-- | Respond with @OK 200@ and some plain text.
responseTextOK :: ByteString -> ServerT IO Response
responseTextOK = return . responseLBS ok200 [(hContentType, "text/plain")] . (`ByteString.append` "\n")
