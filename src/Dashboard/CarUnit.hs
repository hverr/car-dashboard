{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- | Module that ensures connection with the car
module Dashboard.CarUnit (
  CarData(..)
, startFetchingData
) where

import qualified Prelude as P

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forever, when)

import Data.Time.Clock (UTCTime, getCurrentTime)

import Numeric.Units.Dimensional.Prelude

import System.Clock (Clock(Monotonic), TimeSpec(sec, nsec), getTime)

import System.Hardware.ELM327 (connect)
import System.Hardware.ELM327.Car.MAP (mapCar, defaultProperties)
import System.Hardware.ELM327.Connection (close)
import System.Hardware.ELM327.Errors (OBDError)
import qualified System.Hardware.ELM327.Car as Car

-- | A car data point.
type P a = Either OBDError a


-- | Data structure containing all available information.
data CarData = CarData { dataTimestamp :: UTCTime
                       , engineCoolantTemperature :: P (ThermodynamicTemperature Double)
                       , engineFuelRate :: P (VolumeFlow Double)
                       , engineRPM :: P (Frequency Double)
                       , intakeAirTemperature :: P (ThermodynamicTemperature Double)
                       , intakeManifoldAbsolutePressure :: P (Pressure Double)
                       , massAirFlowRate :: P (MassFlow Double)
                       , throttlePosition :: P Double
                       , vehicleSpeed :: P (Velocity Double) }

-- | Start fetching car data.
startFetchingData :: Int          -- ^ The delay between requests in milliseconds
                  -> String       -- ^ The serial port to connect to
                  -> TVar CarData -- ^ A 'TVar' that will be used to write the data to
                  -> IO ()
startFetchingData usDelay port chan = bracket (connect port) close fetch
  where
    fetch con = forever $ ensureDuration $ getData (mapCar con defaultProperties) >>= (atomically . writeTVar chan)
    getData car = CarData <$> getCurrentTime
                          <*> Car.engineCoolantTemperature car
                          <*> Car.engineFuelRate car
                          <*> Car.engineRPM car
                          <*> Car.intakeAirTemperature car
                          <*> Car.intakeManifoldAbsolutePressure car
                          <*> Car.massAirFlowRate car
                          <*> Car.throttlePosition car
                          <*> Car.vehicleSpeed car
    ensureDuration action = do
        begin <- getTime Monotonic
        v <- action
        end <- getTime Monotonic
        let s = fromIntegral $ (sec end P.- sec begin) P.* 1000 P.+ (nsec end P.- nsec begin) `div` 1000000
        when (s P.< usDelay) $ threadDelay (usDelay P.- s)
        return v
