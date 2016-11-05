-- | This module is used to actually play music
module Dashboard.MusicUnit.Player (
  play
, stop
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, tryTakeTMVar, takeTMVar,
                               registerDelay, readTVar, retry)

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)

import System.Log.Logger (infoM, warningM, errorM)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus)
import System.Posix.Signals (signalProcess, sigTERM, sigKILL)
import System.Posix.Types (ProcessID)

import Dashboard.MusicUnit.Monad (MusicUnitT, GlobalState(..), LocalState(..), MusicPlayer(..),
                                  ask, get, modify)
import Dashboard.Paths (getTrackDataFile)
import Dashboard.Settings (Settings(playMusicCmd), completePartialArgumentsNonEmpty)
import qualified Dashboard.MusicUnit.State.Metadata as Metadata

-- | Play the current song.
play :: MonadIO m => MusicUnitT m ()
play = do
    stop
    mpid <- launch
    case mpid of
        Nothing -> return ()
        Just pid -> do
            status <- liftIO newEmptyTMVarIO
            _ <- liftIO . forkIO $ waitFor pid >>= atomically . putTMVar status
            modify $ \s -> s { stateMusicPlayer = Just $ MusicPlayer pid status }
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
    subst _ d "title" = Just $ fromMaybe "Unknown" $ Metadata.track d
    subst _ d "position" = Just $ fromMaybe "null" $ show <$> Metadata.position d
    subst _ _ _ = Nothing

    waitFor pid = maybe (waitFor pid) return =<< getProcessStatus True False pid



-- | Stop the current song.
stop :: MonadIO m => MusicUnitT m ()
stop = do
    mplayer <- stateMusicPlayer <$> get
    case mplayer of
        Nothing -> return ()
        Just player -> do
            status <- liftIO . atomically $ tryTakeTMVar (playerExited player)
            case status of
                Just _ -> return ()
                Nothing -> do
                    sendSigTERM (playerProcessID player)
                    timeout <- liftIO $ registerDelay (3 * 1000 * 1000)
                    stopped <- liftIO . atomically $ do
                        s <- tryTakeTMVar (playerExited player)
                        to <- readTVar timeout
                        case (s, to) of (Just _, _) -> return True
                                        (_, True)   -> return False
                                        (_, False)  -> retry
                    unless stopped $ do
                        sendSigKILL (playerProcessID player)
                        _ <- liftIO . atomically $ takeTMVar (playerExited player)
                        return ()
    modify $ \s -> s { stateMusicPlayer = Nothing }
  where
    sendSigTERM pid = do
        liftIO . infoM "musicunit" $ "Stopping music: sending SIGTERM to " ++ show pid
        liftIO $ signalProcess sigTERM pid
    sendSigKILL pid = do
        liftIO . warningM "musicunit" $ "Stopping music: sending SIGKILL to " ++ show pid
        liftIO $ signalProcess sigKILL pid
