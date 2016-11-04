{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
, MusicUnitT(..)
, startMusicUnit
) where

import Prelude hiding (writeFile)

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, putTMVar, takeTMVar, tryReadTMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, get, put)

import Data.ByteString.Lazy (ByteString, writeFile)

import System.Directory (createDirectoryIfMissing)

import Dashboard.MusicUnit.State (State(..), HasMusicState(..), emptyState,
                                  Metadata, TrackData)
import Dashboard.Paths (getTrackDataFile)
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
queryTrackData = askMusicTrackData >>= liftIO . atomically . tryReadTMVar

-- | Update the track data.
updateTrackData :: (MonadIO m, HasMusicState m, HasSettings m) => TrackData -> ByteString -> m ()
updateTrackData x bs = do
    fp <- getTrackDataFile x
    askSettings >>= liftIO . createDirectoryIfMissing True . musicCacheDir
    liftIO $ writeFile fp bs
    askMusicTrackData >>= liftIO . atomically . flip putTMVar x


-- | Start the music unit
startMusicUnit :: MonadIO m => Settings -> State -> m ()
startMusicUnit settings state = do
    (m, t) <- liftIO . atomically $ do
        a <- takeTMVar (metadata state)
        b <- takeTMVar (trackData state)
        return (a, b)
    let localState = LocalState m t
    let globalState = GlobalState settings state
    _ <- flip runReaderT globalState $ flip runStateT localState $ runMusicUnitT startMusicUnit'
    return ()

-- | State globally accessible
data GlobalState = GlobalState { _serverSettings :: Settings
                               , musicState :: State }

-- | State local to the music unit.
data LocalState = LocalState { stateMetadata :: Metadata
                             , stateTrackData :: TrackData }

-- | Monad transformer for the music unit.
newtype MusicUnitT m a = MusicUnitT { runMusicUnitT :: StateT LocalState (ReaderT GlobalState m) a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadState LocalState, MonadReader GlobalState)

startMusicUnit' :: MonadIO m => MusicUnitT m ()
startMusicUnit' = do
    state <- musicState <$> ask
    m <- stateMetadata <$> get
    t <- stateTrackData <$> get
    if Metadata.playing m && Metadata.songId m == TrackData.songId t
        then error "not implemented: play music" >> return ()
        else error "not implemented: pause music" >> return ()
    let m' = takeTMVar (metadata state)
    let t' = takeTMVar (trackData state)
    update <- liftIO . atomically $ (Left <$> m') <|> (Right <$> t')
    case update of
        Left x -> get >>= put . (\s -> s { stateMetadata = x })
        Right x -> get >>= put . (\s -> s { stateTrackData = x })
    startMusicUnit'
