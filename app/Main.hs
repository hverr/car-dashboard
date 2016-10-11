module Main where

import Dashboard.Server (startServer)
import Dashboard.Settings (defaultSettings)

import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority(..), updateGlobalLogger, rootLoggerName, setHandlers, setLevel)

main :: IO ()
main = do
    -- Setup logging.
    stderrLog <- setFormatter <$> streamHandler stderr INFO <*> pure format
    updateGlobalLogger rootLoggerName $ setLevel INFO
    updateGlobalLogger rootLoggerName $ setHandlers [stderrLog]

    -- Start the main HTTP server.
    startServer 8080 defaultSettings
  where
    format = simpleLogFormatter "[$time $loggername $prio] $msg"
