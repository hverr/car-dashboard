{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | The main monad transformer for the server module.
module Dashboard.Server.Monad (
  -- * Transformers
  ServerT(..)
, ServerState(..)
, defaultState
, runServer
, HasSettings, askSettings

  -- * Re-exports
, liftIO
) where

import Control.Concurrent.STM (TVar, newTVarIO, TMVar, newEmptyTMVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)

import Dashboard.CarUnit (CarData)
import Dashboard.Settings (Settings)
import Dashboard.Server.Errors (ServerErr)

-- | General server state
data ServerState = ServerState { serverSettings :: Settings
                               , carData :: TVar (Maybe CarData)
                               , carError :: TMVar String }

-- | The default starting state of a server
defaultState :: Settings -> IO ServerState
defaultState settings =
    ServerState <$> pure settings
                <*> newTVarIO Nothing
                <*> newEmptyTMVarIO

-- | Transformer for server methods.
newtype ServerT m a = ServerT { runServerT :: ReaderT ServerState (ExceptT ServerErr m) a }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerState, MonadError ServerErr)

-- | Run a server with given state.
runServer :: ServerState -> ServerT m a -> m (Either ServerErr a)
runServer state = runExceptT . flip runReaderT state . runServerT

-- | Type class to get settings quickly
class Monad m => HasSettings m where
    askSettings :: m Settings

-- | The server transformer has settings
instance MonadReader ServerState m => HasSettings m where
    askSettings = serverSettings <$> ask
