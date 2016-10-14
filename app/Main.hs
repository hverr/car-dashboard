module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, putTMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever)

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
import System.Log.Logger (Priority(..), updateGlobalLogger, rootLoggerName, setHandlers, setLevel)

import System.Hardware.ELM327.Connection (close)
import System.Hardware.ELM327.Car.MAP (mapCar, defaultProperties)
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
    stderrLog <- setFormatter <$> streamHandler stderr INFO <*> pure format
    updateGlobalLogger rootLoggerName $ setLevel INFO
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
carUnit ct state = forever $ do
    v <- try fetcher
    either handleExc return v
  where
    fetcher = bracket connect close $ \con -> do
        let car = mapCar con defaultProperties
        let carData' = carData state
        startFetchingData 500 car carData'

    connect = connect' ct
    connect' ConnectionTypeSimulator = Simulator.connect $ defaultSimulator stoppedCarBus
    connect' (ConnectionTypeActualDevice dev) = ELM327.connect dev

    handleExc :: SomeException -> IO ()
    handleExc e = atomically . putTMVar (carError state) $ "Car Unit Error: " ++ show e
