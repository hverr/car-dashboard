module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import Options.Applicative (Parser, execParser,
                            flag, strOption,
                            help, long, value,
                            info, progDesc,
                            (<>))
import Options.Applicative.Extra (helper)

import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority(..), updateGlobalLogger,
                          rootLoggerName, setHandlers, setLevel,
                          noticeM, errorM)

import System.Hardware.ELM327.Car (runCarT)
import System.Hardware.ELM327.Car.MAP (mapCar, defaultProperties)
import System.Hardware.ELM327.Commands (AT(..), Protocol(..))
import System.Hardware.ELM327.Connection (conLog, fileLog, withCon, at, close')
import System.Hardware.ELM327.Simulator (defaultSimulator)
import System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 (stoppedCarBus)
import qualified System.Hardware.ELM327 as ELM327
import qualified System.Hardware.ELM327.Simulator as Simulator

import Dashboard.CarUnit (startFetchingData)
import Dashboard.Server (startServer)
import Dashboard.Server.Monad (ServerState(..), defaultState)
import Dashboard.Settings (defaultSettings)

data ConnectionType = ConnectionTypeActualDevice FilePath
                    | ConnectionTypeSimulator

connectionType :: Parser ConnectionType
connectionType = flag ConnectionTypeActualDevice (const ConnectionTypeSimulator) m <*> serialPort
    where m = long "simulator" <> help "Connect to the simulator instead of a serial port"

serialPort :: Parser FilePath
serialPort = strOption m
    where m = long "port" <> value "/dev/ttyUSB0" <> help "The serial port to connect to"

main :: IO ()
main = execParser i >>= main'
    where i = info p $ progDesc "Run the car dashboard"
          p = helper <*> connectionType

main' :: ConnectionType -> IO ()
main' conType = do
    -- Setup logging.
    stderrLog <- setFormatter <$> streamHandler stderr DEBUG <*> pure format
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [stderrLog]

    -- Initialize the global state
    state <- defaultState defaultSettings

    -- Start server components
    _ <- forkIO $ carUnit conType state
    startServer 8080 state
  where
    format = simpleLogFormatter "[$time $loggername $prio] $msg"

-- | Manage the car unit
carUnit :: ConnectionType -> ServerState -> IO ()
carUnit ct state = forever $ try (bracket connect close' fetcher) >>= either handleExc return
  where
    fetcher con = do
        let car = mapCar defaultProperties
        let carData' = carData state
        r <- withCon con $ do
            liftIO $ noticeM "carunit" $ "Selecting protocol " ++ show (ATSelectProtocol AutomaticProtocol)
            _ <- at (ATSelectProtocol AutomaticProtocol)
            runCarT $ startFetchingData 500 car carData'
        case r of
            Right _ -> fetcher con
            Left e -> handleErr e

    connect = do
        l <- fileLog "car-dashboard.serial"
        c <- connect' ct
        return c { conLog = l }

    connect' ConnectionTypeSimulator = Simulator.connect $ defaultSimulator stoppedCarBus
    connect' (ConnectionTypeActualDevice dev) = do
        noticeM "carunit" $ "Connecting to " ++ dev
        con <- ELM327.connect dev
        case con of
            Right con' -> return con'
            Left err -> do
                errorM "carunit" $ "Cannot connect, retrying: " ++ show err
                connect

    handleErr e = do
        errorM "carunit" $ show e
        errorM "carunit" "Immediately retrying..."

    handleExc :: SomeException -> IO ()
    handleExc e = do
        let msg = "Unexpected Car Unit Error: " ++ show e
        atomically $ writeTVar (carError state) (Just msg)
        errorM "carunit" $ show e
        threadDelay $ 5*1000*1000
