{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Module containg the 'MusicUnit' monad and transformer types.
module Dashboard.MusicUnit.Monad (
  -- * The base monad
  MusicUnitT(..)
, runMusicUnit

  -- * State structures
, GlobalState(..)
, LocalState(..)

  -- * Re-exports
, ask, get, put, modify
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, get, put, modify)

import System.Posix.Types (ProcessID)

import Dashboard.MusicUnit.State (State(..), Metadata, TrackData)
import Dashboard.Settings (Settings, HasSettings(..))

-- | State globally accessible
data GlobalState = GlobalState { serverSettings :: Settings
                               , musicState :: State }

-- | State local to the music unit.
data LocalState = LocalState { stateMetadata :: Metadata
                             , stateTrackData :: TrackData
                             , statePlayProcessID :: Maybe ProcessID }

-- | Monad transformer for the music unit.
newtype MusicUnitT m a = MusicUnitT { runMusicUnitT :: StateT LocalState (ReaderT GlobalState m) a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadState LocalState, MonadReader GlobalState)

-- | Get global settings from 'MusicUnitT'
instance Monad m => HasSettings (MusicUnitT m) where
    askSettings = serverSettings <$> ask

-- | Run a 'MusicUnitT'
runMusicUnit :: Monad m => MusicUnitT m a -> LocalState -> GlobalState -> m a
runMusicUnit action local global = fst <$> runReaderT (runStateT (runMusicUnitT action) local) global
