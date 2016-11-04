{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Module containg the 'MusicUnit' monad and transformer types.
module Dashboard.MusicUnit.Monad (
  -- * The base monad
  MusicUnitT(..)
, runMusicUnit

  -- * State structures
, GlobalState(..)
, LocalState(..)

  -- * Re-exports
, ask, get, put
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, get, put)

import Dashboard.MusicUnit.State (State(..), Metadata, TrackData)
import Dashboard.Settings (Settings)

-- | State globally accessible
data GlobalState = GlobalState { _serverSettings :: Settings
                               , musicState :: State }

-- | State local to the music unit.
data LocalState = LocalState { stateMetadata :: Metadata
                             , stateTrackData :: TrackData }

-- | Monad transformer for the music unit.
newtype MusicUnitT m a = MusicUnitT { runMusicUnitT :: StateT LocalState (ReaderT GlobalState m) a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadState LocalState, MonadReader GlobalState)

-- | Run a 'MusicUnitT'
runMusicUnit :: Monad m => MusicUnitT m a -> LocalState -> GlobalState -> m a
runMusicUnit action local global = fst <$> runReaderT (runStateT (runMusicUnitT action) local) global
