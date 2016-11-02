{-# LANGUAGE DeriveGeneric #-}
-- | Module containing state data structures for the music unit.
module Dashboard.MusicUnit.State (
  -- * Global State
  State(..)
, HasMusicState(..)
, emptyState

  -- * Metadata
, Metadata(..)

  -- * Track Data
, TrackData(..)
) where

import GHC.Generics (Generic)

import Control.Concurrent.STM (TMVar, newEmptyTMVarIO)
import Data.Aeson (ToJSON, FromJSON)

-- | State of the music unit
data State = State { metadata :: TMVar Metadata }
                   deriving (Generic)

-- | Default empty 'State'
emptyState :: IO State
emptyState = State <$> newEmptyTMVarIO

-- | Metadata about the music
data Metadata = Metadata { songId :: Int
                         , playing :: Bool
                         , position :: Maybe Int
                         , track :: Maybe String
                         , artist :: Maybe String }
                         deriving (Generic)

-- | Convert 'Metadata' to JSON
instance FromJSON Metadata where

-- | Convert JSON to 'Metadata'
instance ToJSON Metadata where

-- | The actual music data of a track
data TrackData = TrackData { trackSongId :: Int
                           , trackF :: String
                           , trackFilePath :: FilePath }

-- | Type class to get music state quickly
class Monad m => HasMusicState m where
    askMusicState :: m State

    askMusicMetadata :: m (TMVar Metadata)
    askMusicMetadata = metadata <$> askMusicState
