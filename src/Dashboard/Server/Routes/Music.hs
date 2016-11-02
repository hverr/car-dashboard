{-# LANGUAGE FlexibleContexts #-}
module Dashboard.Server.Routes.Music where

import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai (pathInfo, requestMethod)

import Dashboard.MusicUnit (queryMetadata, updateMetadata)
import Dashboard.Server.Routes (Route(..), responseTextOK)
import Dashboard.Server.Routes.Json (jsonify, objectify)

-- | All installed routes
routes :: [Route]
routes = [serveQueryMetadata, serveUpdateMetadata]

-- | Get the current music metadata
serveQueryMetadata :: Route
serveQueryMetadata = Route pred' (jsonify f)
  where
    pred' x = pred1 x && pred2 x
    pred1 = (== ["api", "music", "metadata"]) . pathInfo
    pred2 = (== methodGet) . requestMethod
    f _ = queryMetadata

serveUpdateMetadata :: Route
serveUpdateMetadata = Route pred' (objectify f)
  where
    pred' x = pred1 x && pred2 x
    pred1 = (== ["api", "music", "metadata"]) . pathInfo
    pred2 = (== methodPost) . requestMethod
    f x = updateMetadata x >> responseTextOK "OK"
