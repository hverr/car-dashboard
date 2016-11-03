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

import Control.Concurrent.STM (TMVar, newEmptyTMVarIO)

import Dashboard.MusicUnit.State.Metadata (Metadata)
import Dashboard.MusicUnit.State.TrackData (TrackData)

-- | State of the music unit
data State = State { metadata :: TMVar Metadata
                   , trackData :: TMVar TrackData }
                   deriving (Generic)

-- | Default empty 'State'
emptyState :: IO State
emptyState = State <$> newEmptyTMVarIO <*> newEmptyTMVarIO

-- | Type class to get music state quickly
class Monad m => HasMusicState m where
    askMusicState :: m State

    askMusicMetadata :: m (TMVar Metadata)
    askMusicMetadata = metadata <$> askMusicState

    askMusicTrackData :: m (TMVar TrackData)
    askMusicTrackData = trackData <$> askMusicState
