{-# LANGUAGE DeriveGeneric #-}
-- | Module containing the 'TrackData' data structure.
module Dashboard.MusicUnit.State.TrackData where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import Dashboard.MusicUnit.Files (FileExtension)

-- | Raw track data.
data TrackData = TrackData { songId :: Int
                           , fileExtension :: FileExtension }
                         deriving (Show, Generic)

-- | Convert 'TrackData' to JSON
instance FromJSON TrackData where

-- | Convert JSON to 'TrackData'
instance ToJSON TrackData where
