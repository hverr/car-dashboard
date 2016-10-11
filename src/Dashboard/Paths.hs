{-# LANGUAGE FlexibleContexts #-}
-- | Module with functions to handle paths and data files.
module Dashboard.Paths where

import Control.Monad.Reader (ask)
import System.FilePath ((</>))

import Dashboard.Settings (Settings(htmlRoot, nodeModulesRoot))
import Dashboard.Server.Monad (HasSettings)

-- | Get the path to an HTML file.
getHtmlFile :: HasSettings m => FilePath -> m FilePath
getHtmlFile f = (</> f) . htmlRoot <$> ask

-- | Get the path to a node module file.
getNodeModuleFile :: HasSettings m => FilePath -> m FilePath
getNodeModuleFile f = (</> f) . nodeModulesRoot <$> ask
