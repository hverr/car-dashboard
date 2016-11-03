{-# LANGUAGE FlexibleContexts #-}
module Dashboard.Server.Routes.Music where

import Control.Lens ((^?))
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as Text

import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai (pathInfo, requestMethod, lazyRequestBody)

import Dashboard.MusicUnit (queryMetadata, updateMetadata, queryTrackData, updateTrackData)
import Dashboard.MusicUnit.Files (FileExtension, fileExtension)
import Dashboard.MusicUnit.State.TrackData (TrackData(TrackData))
import Dashboard.Server.Errors (throwError, invalidRoute)
import Dashboard.Server.Monad (liftIO)
import Dashboard.Server.Routes (Route(..), responseTextOK)
import Dashboard.Server.Routes.Json (jsonify, objectify)

-- | All installed routes
routes :: [Route]
routes = [serveQueryMetadata, serveUpdateMetadata,
          serveQueryTrackData, serveUpdateTrackData]

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

serveQueryTrackData :: Route
serveQueryTrackData = Route pred' (jsonify f)
  where
    pred' x = pred1 x && pred2 x
    pred1 = (== ["api", "music", "track_data"]) . pathInfo
    pred2 = (== methodGet) . requestMethod
    f _ = queryTrackData


serveUpdateTrackData :: Route
serveUpdateTrackData = Route pred' f
  where
    pred' x | Just _ <- extract (pathInfo x) = requestMethod x == methodPost
            | otherwise = False
    f x | Just (songId, fileExt) <- extract (pathInfo x) = do
            liftIO (lazyRequestBody x) >>= updateTrackData (TrackData songId fileExt)
            responseTextOK "OK"
        | otherwise = throwError invalidRoute

    extract :: [Text] -> Maybe (Int, FileExtension)
    extract xs
        | ["api", "music", "track_data", as, bs] <- xs =
            let a = readMaybe (Text.unpack as)
                b = Text.unpack bs ^? fileExtension
            in
            case (a, b) of (Just a', Just b') -> Just (a', b')
                           _ -> Nothing
        | otherwise = Nothing
