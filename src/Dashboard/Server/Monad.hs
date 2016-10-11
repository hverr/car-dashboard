{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | The main monad transformer for the server module.
module Dashboard.Server.Monad (
  -- * Transformers
  ServerT(..)
, runServer
, HasSettings

  -- * Re-exports
, liftIO
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)

import Dashboard.Settings (Settings)
import Dashboard.Server.Errors (ServerErr)

-- | Transformer for server methods.
newtype ServerT m a = ServerT { runServerT :: ReaderT Settings (ExceptT ServerErr m) a }
                    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Settings, MonadError ServerErr)

-- | Run a server with given state.
runServer :: Settings -> ServerT m a -> m (Either ServerErr a)
runServer settings = runExceptT . flip runReaderT settings . runServerT

-- | Type alias for a monad with access to settings.
type HasSettings m = MonadReader Settings m
