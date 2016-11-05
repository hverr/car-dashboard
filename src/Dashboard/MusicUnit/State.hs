{-# LANGUAGE DeriveGeneric #-}
-- | Module containing state data structures for the music unit.
module Dashboard.MusicUnit.State (
  -- * Global State
  State(..)
, HasMusicState(..)
, emptyState

  -- * Re-exported data structures
, Metadata
, TrackData
) where

import GHC.Generics (Generic)

import Control.Concurrent.STM (TMVar, TVar, newEmptyTMVarIO, newTVarIO)

import Dashboard.MusicUnit.State.Metadata (Metadata)
import Dashboard.MusicUnit.State.TrackData (TrackData)

-- | State of the music unit
data State = State { metadata :: TMVar Metadata
                   , cachedTrackData :: TVar (Maybe TrackData)
                   , newTrackData :: TMVar TrackData }
                   deriving (Generic)

-- | Default empty 'State'
emptyState :: IO State
emptyState = State <$> newEmptyTMVarIO <*> newTVarIO Nothing <*> newEmptyTMVarIO

-- | Type class to get music state quickly
class Monad m => HasMusicState m where
    askMusicState :: m State

    askMusicMetadata :: m (TMVar Metadata)
    askMusicMetadata = metadata <$> askMusicState

    askMusicCachedTrackData :: m (TVar (Maybe TrackData))
    askMusicCachedTrackData = cachedTrackData <$> askMusicState

    askMusicNewTrackData :: m (TMVar TrackData)
    askMusicNewTrackData = newTrackData <$> askMusicState
