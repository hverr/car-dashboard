{-# LANGUAGE FlexibleContexts #-}
-- | Module with functions to handle paths and data files.
module Dashboard.Paths where

import Control.Lens (re, (^.))

import System.FilePath ((</>))

import Dashboard.MusicUnit.Files (fileExtension)
import Dashboard.MusicUnit.State.TrackData (TrackData(TrackData))
import Dashboard.Server.Monad (HasSettings, askSettings)
import Dashboard.Settings (Settings(htmlRoot, nodeModulesRoot, musicCacheDir))

-- | Get the path to an HTML file.
getHtmlFile :: HasSettings m => FilePath -> m FilePath
getHtmlFile f = (</> f) . htmlRoot <$> askSettings

-- | Get the path to a node module file.
getNodeModuleFile :: HasSettings m => FilePath -> m FilePath
getNodeModuleFile f = (</> f) . nodeModulesRoot <$> askSettings

-- | Get the path to the track data of a music file.
getTrackDataFile :: HasSettings m => TrackData -> m FilePath
getTrackDataFile (TrackData sid s) =
    let fn = show sid ++ "." ++ s ^. re fileExtension in
    (</> fn) . musicCacheDir <$> askSettings
