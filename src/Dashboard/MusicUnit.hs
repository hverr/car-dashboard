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

  -- * Main logic
, startMusicUnit
) where

import Prelude hiding (writeFile)

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, atomically, putTMVar, takeTMVar, tryReadTMVar, readTVar, writeTVar)
import Control.Exception (throwIO, catch)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Lazy (ByteString, writeFile)

import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.IO.Error (isDoesNotExistError)

import Dashboard.MusicUnit.State (State(..), HasMusicState(..), emptyState,
                                  Metadata, TrackData)
import Dashboard.Paths (getTrackDataFile)
import Dashboard.MusicUnit.Monad (MusicUnitT, LocalState(..), GlobalState(..), runMusicUnit, ask, get, put)
import Dashboard.MusicUnit.Player (play, stop)
import Dashboard.Server.Monad (HasSettings(..))
import Dashboard.Settings (Settings, musicCacheDir)
import qualified Dashboard.MusicUnit.State.Metadata as Metadata
import qualified Dashboard.MusicUnit.State.TrackData as TrackData

-- | Query the current music metadata.
queryMetadata :: (MonadIO m, HasMusicState m) => m (Maybe Metadata)
queryMetadata = askMusicMetadata >>= liftIO . atomically . tryReadTMVar

-- | Update the music metadata.
updateMetadata :: (MonadIO m, HasMusicState m) => Metadata -> m ()
updateMetadata x = askMusicMetadata >>= liftIO . atomically . flip putTMVar x

-- | Query the current track data.
queryTrackData :: (MonadIO m, HasMusicState m) => m (Maybe TrackData)
queryTrackData = askMusicCachedTrackData >>= liftIO . atomically . readTVar

-- | Update the track data.
updateTrackData :: (MonadIO m, HasMusicState m, HasSettings m) => TrackData -> ByteString -> m ()
updateTrackData x bs = do
    -- Remove old data
    maybeOld <- askMusicCachedTrackData >>= liftIO . atomically . readTVar
    maybe (return ()) (getTrackDataFile >=> removeIfExists) maybeOld

    -- Write track data
    fp <- getTrackDataFile x
    askSettings >>= liftIO . createDirectoryIfMissing True . musicCacheDir
    liftIO $ writeFile fp bs
    askMusicNewTrackData >>= liftIO . atomically . flip putTMVar x
    askMusicCachedTrackData >>= liftIO . atomically . flip writeTVar (Just x)

  where
    removeIfExists fp = liftIO $ removeFile fp `catch` ignoreDoesNotExistError


-- | Start the music unit
startMusicUnit :: MonadIO m => Settings -> State -> m ()
startMusicUnit settings state = clearCache >> loop Nothing Nothing
  where
    clearCache = liftIO $ removeDirectoryRecursive (musicCacheDir settings) `catch` ignoreDoesNotExistError
    loop (Just m) (Just t) = do
        let localState = LocalState m t Nothing
        let globalState = GlobalState settings state
        runMusicUnit startMusicUnit' localState globalState
    loop m t = do
        update <- liftIO . atomically $ takeMetadataOrTrackData state
        case update of
            Left x -> loop (Just x) t
            Right x -> loop m (Just x)

startMusicUnit' :: MonadIO m => MusicUnitT m ()
startMusicUnit' = do
    state <- musicState <$> ask
    m <- stateMetadata <$> get
    t <- stateTrackData <$> get
    if Metadata.playing m && Metadata.songId m == TrackData.songId t
        then play
        else stop
    update <- liftIO . atomically $ takeMetadataOrTrackData state
    case update of
        Left x -> get >>= put . (\s -> s { stateMetadata = x })
        Right x -> get >>= put . (\s -> s { stateTrackData = x })
    startMusicUnit'

-- | Read-in either new metadata or new track data.
takeMetadataOrTrackData :: State -> STM (Either Metadata TrackData)
takeMetadataOrTrackData state =
    let m = takeTMVar (metadata state)
        t = takeTMVar (newTrackData state)
    in (Left <$> m) <|> (Right <$> t)

-- | Ignore 'does note exist' errors
ignoreDoesNotExistError :: IOError -> IO ()
ignoreDoesNotExistError e | isDoesNotExistError e = return ()
                          | otherwise = throwIO e
