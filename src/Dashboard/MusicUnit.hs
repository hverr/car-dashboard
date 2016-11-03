-- | Unit managing the music component of the application.
--
-- For more functions to manage state, see 'Dashboard.MusicUnit.State'.
module Dashboard.MusicUnit (
  -- * State
  State(..)
, HasMusicState(..)
, emptyState

  -- * Updating and queryingstate
, queryMetadata
, updateMetadata
, queryTrackData
, updateTrackData
) where

import Prelude hiding (writeFile)

import Control.Concurrent.STM (atomically, putTMVar, tryReadTMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Lazy (ByteString, writeFile)

import System.Directory (createDirectoryIfMissing)

import Dashboard.MusicUnit.State (State(..), HasMusicState(..), emptyState,
                                  Metadata, TrackData)
import Dashboard.Paths (getTrackDataFile)
import Dashboard.Server.Monad (HasSettings(..))
import Dashboard.Settings (musicCacheDir)

-- | Query the current music metadata.
queryMetadata :: (MonadIO m, HasMusicState m) => m (Maybe Metadata)
queryMetadata = askMusicMetadata >>= liftIO . atomically . tryReadTMVar

-- | Update the music metadata.
updateMetadata :: (MonadIO m, HasMusicState m) => Metadata -> m ()
updateMetadata x = askMusicMetadata >>= liftIO . atomically . flip putTMVar x

-- | Query the current track data.
queryTrackData :: (MonadIO m, HasMusicState m) => m (Maybe TrackData)
queryTrackData = askMusicTrackData >>= liftIO . atomically . tryReadTMVar

-- | Update the track data.
updateTrackData :: (MonadIO m, HasMusicState m, HasSettings m) => TrackData -> ByteString -> m ()
updateTrackData x bs = do
    fp <- getTrackDataFile x
    askSettings >>= liftIO . createDirectoryIfMissing True . musicCacheDir
    liftIO $ writeFile fp bs
    askMusicTrackData >>= liftIO . atomically . flip putTMVar x
