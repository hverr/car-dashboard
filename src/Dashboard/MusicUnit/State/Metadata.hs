{-# LANGUAGE DeriveGeneric #-}
-- | Module containing the 'Metadata' data structure.
module Dashboard.MusicUnit.State.Metadata where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

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
