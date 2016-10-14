{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Contains routes for serving API content.
module Dashboard.Server.Routes.Api (
  routes
, serveCarData
, serveCarErrors
) where

import Prelude hiding (error)

import Control.Concurrent.STM (atomically, readTVar, swapTVar,
                               registerDelay, retry)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Network.Wai (pathInfo)

import Dashboard.Server.Monad (ask, carError, carData)
import Dashboard.Server.Routes (Route(..))
import Dashboard.Server.Routes.Json (jsonify)

-- | All installed routes
routes :: [Route]
routes = [serveCarData, serveCarErrors]

-- | Serve the car data
serveCarData :: Route
serveCarData = Route pred' (jsonify f)
  where
    pred' = (== ["api", "engine", "status"]) . pathInfo
    f _ = ask >>= liftIO . atomically . readTVar . carData

-- | Serve the car unit errors
serveCarErrors :: Route
serveCarErrors = Route pred' (jsonify f)
  where
    pred' = (== ["api", "engine", "errors"]) . pathInfo
    f _ = do
        err <- readError
        return CarError { error = err }
    readError = do
        state <- ask
        timeout <- liftIO $ registerDelay (10*1000*1000)
        liftIO . atomically $ do
            err <- swapTVar (carError state) Nothing
            to <- readTVar timeout
            case (err, to) of
                (Nothing, True) -> return Nothing
                (Nothing, False) -> retry
                (Just e, _) -> return $ Just e

-- | Wrap errors in a JSON data structure
data CarError = CarError { error :: Maybe String } deriving (Generic)
instance ToJSON CarError where
