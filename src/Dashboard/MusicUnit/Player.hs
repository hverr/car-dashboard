-- | This module is used to actually play music
module Dashboard.MusicUnit.Player (
  play
, stop
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)

import System.Log.Logger (infoM, errorM)
import System.Posix.Process (forkProcess, executeFile)
import System.Posix.Signals (signalProcess, sigINT)
import System.Posix.Types (ProcessID)

import Dashboard.MusicUnit.Monad (MusicUnitT, GlobalState(..), LocalState(..),
                                  ask, get, modify)
import Dashboard.Paths (getTrackDataFile)
import Dashboard.Settings (Settings(playMusicCmd), completePartialArgumentsNonEmpty)
import qualified Dashboard.MusicUnit.State.Metadata as Metadata

-- | Play the current song.
play :: MonadIO m => MusicUnitT m ()
play = do
    stop
    pid <- launch
    modify $ \s -> s { statePlayProcessID = pid }
  where
    launch :: MonadIO m => MusicUnitT m (Maybe ProcessID)
    launch = do
        mp <- program
        case mp of
            Nothing -> do
                liftIO . errorM "musicunit" $ "Could not instantiate music player command template"
                return Nothing
            Just p -> do
                liftIO . infoM "musicunit" $ "Playing music: " ++ show p
                liftIO $ Just <$> forkProcess (exec p)

    exec (fp :| args) = executeFile fp True args Nothing

    program :: Monad m => MusicUnitT m (Maybe (NonEmpty String))
    program = do
        fp <- get >>= getTrackDataFile . stateTrackData
        md <- stateMetadata <$> get
        template <- playMusicCmd . serverSettings <$> ask
        return $ completePartialArgumentsNonEmpty (subst fp md) template

    subst x _ "file" = Just x
    subst _ d "artist" = Just $ fromMaybe "Unknown" $ Metadata.artist d
    subst _ d "title" = Just $ fromMaybe "Unknown" $ Metadata.artist d
    subst _ _ _ = Nothing



-- | Stop the current song.
stop :: MonadIO m => MusicUnitT m ()
stop = do
    pid <- statePlayProcessID <$> get
    maybe (return ()) stop' pid
  where
    stop' pid = do
        liftIO . infoM "musicunit" $ "Stopping music: sending SIGINT to " ++ show pid
        liftIO $ signalProcess sigINT pid
        undefined
