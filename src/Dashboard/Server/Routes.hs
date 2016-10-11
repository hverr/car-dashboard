-- | This module contains the HTTP routes
module Dashboard.Server.Routes where

import Network.Wai (Request, Response)

import Dashboard.Server.Monad (ServerT)

-- | A generic route is a predicate and a response generating function.
data Route = Route (Request -> Bool) (Request -> ServerT IO Response)

-- | Match a route from a list of installed routes.
match :: [Route] -> Request -> Maybe (Request -> ServerT IO Response)
match [] _ = Nothing
match (Route pred' f:xs) req = if pred' req then Just f else match xs req
