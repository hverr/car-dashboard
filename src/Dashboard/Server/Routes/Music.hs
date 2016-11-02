{-# LANGUAGE FlexibleContexts #-}
module Dashboard.Server.Routes.Music where

import Control.Lens ((^?))
import Data.Text (Text)
import qualified Data.Text as Text

import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai (pathInfo, requestMethod, lazyRequestBody)

import Dashboard.MusicUnit (queryMetadata, updateMetadata, updateTrackData)
import Dashboard.MusicUnit.Files (FileExtension, fileExtension)
import Dashboard.Server.Errors (throwError, invalidRoute)
import Dashboard.Server.Monad (liftIO)
import Dashboard.Server.Routes (Route(..), responseTextOK)
import Dashboard.Server.Routes.Json (jsonify, objectify)

-- | All installed routes
routes :: [Route]
routes = [serveQueryMetadata, serveUpdateMetadata, serveUpdateTrackData]

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

serveUpdateTrackData :: Route
serveUpdateTrackData = Route pred' f
  where
    pred' x | Just _ <- extract (pathInfo x) = True
            | otherwise = False
    f x | Just (songId, fileExt) <- extract (pathInfo x) = do
            liftIO (lazyRequestBody x) >>= updateTrackData songId fileExt
            responseTextOK "OK"
        | otherwise = throwError invalidRoute

    extract :: [Text] -> Maybe (Int, FileExtension)
    extract xs
        | ["api", "music", "track_data", as, bs] <- xs =
            let a = read (Text.unpack as)
                b = Text.unpack bs ^? fileExtension
            in
            case (a, b) of (Just a', Just b') -> Just (a', b')
                           _ -> Nothing
        | otherwise = Nothing
