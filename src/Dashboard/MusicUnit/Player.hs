-- | This module is used to actually play music
module Dashboard.MusicUnit.Player where

import Control.Monad.IO.Class (MonadIO)

import Dashboard.MusicUnit.Monad (MusicUnitT)

-- | Play the current song.
play :: MonadIO m => MusicUnitT m ()
play = undefined

-- | Stop the current song.
stop :: MonadIO m => MusicUnitT m ()
stop = undefined
