module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Lens ((^?))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import Options.Applicative (Parser, execParser,
                            flag, strOption,
                            help, long, short, value,
                            info, progDesc,
                            (<>))
import Options.Applicative.Extra (helper)

import System.Exit (die)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority(..), updateGlobalLogger,
                          rootLoggerName, setHandlers, setLevel,
                          noticeM, errorM)

import System.Hardware.ELM327.Car (runCarT)
import System.Hardware.ELM327.Car.MAP (mapCar, defaultProperties)
import System.Hardware.ELM327.Commands (AT(..), Protocol(..), protocol)
import System.Hardware.ELM327.Connection (conLog, fileLog, withCon, at, close')
import System.Hardware.ELM327.Simulator (defaultSimulator)
import System.Hardware.ELM327.Simulator.OBDBus.VWPolo2007 (stoppedCarBus)
import qualified System.Hardware.ELM327 as ELM327
import qualified System.Hardware.ELM327.Simulator as Simulator

import Dashboard.CarUnit (startFetchingData)
import Dashboard.Server (startServer)
import Dashboard.Server.Monad (ServerState(..), defaultState)
import Dashboard.Settings (defaultSettings)

data Options = Options ConnectionType (Maybe Protocol)

data ConnectionType = ConnectionTypeActualDevice FilePath
                    | ConnectionTypeSimulator

connectionType :: Parser ConnectionType
connectionType = flag ConnectionTypeActualDevice (const ConnectionTypeSimulator) m <*> serialPort
    where m = long "simulator" <> help "Connect to the simulator instead of a serial port"

serialPort :: Parser FilePath
serialPort = strOption m
    where m = long "port" <> value "/dev/ttyUSB0" <> help "The serial port to connect to"

protocolOpt :: Parser (Maybe Protocol)
protocolOpt = (^? protocol) <$> strOption m
    where m = long "protocol" <> short 'p' <> value "0" <> help h
          h = "The OBD protocol, see the SP command in the ELM327 datasheet"

options :: Parser Options
options = Options <$> connectionType <*> protocolOpt

main :: IO ()
main = execParser i >>= main'
    where i = info p $ progDesc "Run the car dashboard"
          p = helper <*> options

main' :: Options -> IO ()
main' (Options conType mProto) = do
    proto <- maybe (die "Invalid protocol (-p|--protocol)") return mProto

    -- Setup logging.
    stderrLog <- setFormatter <$> streamHandler stderr DEBUG <*> pure format
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [stderrLog]

    -- Initialize the global state
    state <- defaultState defaultSettings

    -- Start server components
    _ <- forkIO $ carUnit conType proto state
    startServer 8080 state
  where
    format = simpleLogFormatter "[$time $loggername $prio] $msg"

-- | Manage the car unit
carUnit :: ConnectionType -> Protocol -> ServerState -> IO ()
carUnit ct proto state = forever $ try (bracket connect close' fetcher) >>= either handleExc return
  where
    fetcher con = do
        let car = mapCar defaultProperties
        let carData' = carData state
        r <- withCon con $ do
            liftIO $ noticeM "carunit" $ "Selecting protocol " ++ show proto
            _ <- at (ATSelectProtocol proto)
            runCarT $ startFetchingData 3000 car carData'
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
