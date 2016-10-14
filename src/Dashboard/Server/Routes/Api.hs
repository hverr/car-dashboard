{-# LANGUAGE FlexibleContexts #-}
-- | Contains routes for serving API content.
module Dashboard.Server.Routes.Api where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad.IO.Class (liftIO)

import Network.Wai (pathInfo)

import Dashboard.Server.Monad (ask, carData)
import Dashboard.Server.Routes (Route(..))
import Dashboard.Server.Routes.Json (jsonify)

-- | All installed routes
routes :: [Route]
routes = [serveEngineData]

-- | Serve the engine data
serveEngineData :: Route
serveEngineData = Route pred' (jsonify f)
  where
    pred' = (== ["api", "engine", "status"]) . pathInfo
    f _ = ask >>= liftIO . atomically . readTVar . carData
