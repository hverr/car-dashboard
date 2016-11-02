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
, updateTrackData
) where

import Control.Concurrent.STM (atomically, putTMVar, tryReadTMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Lazy (ByteString)

import Dashboard.MusicUnit.State (State(..), HasMusicState(..), emptyState,
                                  Metadata)

-- | Query the current music metadata.
queryMetadata :: (MonadIO m, HasMusicState m) => m (Maybe Metadata)
queryMetadata = askMusicMetadata >>= liftIO . atomically . tryReadTMVar

-- | Update the music metadata.
updateMetadata :: (MonadIO m, HasMusicState m) => Metadata -> m ()
updateMetadata x = askMusicMetadata >>= liftIO . atomically . flip putTMVar x

-- | Update the track data.
updateTrackData :: (MonadIO m, HasMusicState m) => Int -> String -> ByteString -> m ()
updateTrackData = undefined
