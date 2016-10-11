-- | Log in the 'ServerT' monad transformer.
--
-- Based on 'hslogger'.
module Dashboard.Server.Logger (
  -- * Counter parts of 'System.Log.Logger' functions
  debugS
, infoS
, noticeS
, warningS
, errorS
, criticalS
, alertS
, emergencyS
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Log.Logger

-- | Counterpart of 'debugS'.
debugS :: MonadIO m => String -> String -> m ()
debugS = liftIO |. debugM

-- | Counterpart of 'infoS'.
infoS :: MonadIO m => String -> String -> m ()
infoS = liftIO |. infoM

-- | Counterpart of 'noticeM'.
noticeS :: MonadIO m => String -> String -> m ()
noticeS = liftIO |. noticeM

-- | Counterpart of 'warningM'.
warningS :: MonadIO m => String -> String -> m ()
warningS = liftIO |. warningM

-- | Counterpart of 'errorM'.
errorS :: MonadIO m => String -> String -> m ()
errorS = liftIO |. errorM

-- | Counterpart of 'criticalM'.
criticalS :: MonadIO m => String -> String -> m ()
criticalS = liftIO |. criticalM

-- | Counterpart of 'alertM'.
alertS :: MonadIO m => String -> String -> m ()
alertS = liftIO |. alertM

-- | Counterpart of 'emergencyM'.
emergencyS :: MonadIO m => String -> String -> m ()
emergencyS = liftIO |. emergencyM

-- | Helper for function composition
(|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(|.) f g x = f . g x
